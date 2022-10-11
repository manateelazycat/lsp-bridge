import gzip
import hashlib
import os
import re
import warnings
from struct import unpack

import six


class _StarDictIfo(object):
    """
    The .ifo file has the following format:

    StarDict's dict ifo file
    version=2.4.2
    [options]

    Note that the current "version" string must be "2.4.2" or "3.0.0".  If it's not,
    then StarDict will refuse to read the file.
    If version is "3.0.0", StarDict will parse the "idxoffsetbits" option.

    [options]
    ---------
    In the example above, [options] expands to any of the following lines
    specifying information about the dictionary.  Each option is a keyword
    followed by an equal sign, then the value of that option, then a
    newline.  The options may be appear in any order.

    Note that the dictionary must have at least a bookname, a wordcount and a 
    idxfilesize, or the load will fail.  All other information is optional.  All 
    strings should be encoded in UTF-8.

    Available options:

    bookname=      // required
    wordcount=     // required
    synwordcount=  // required if ".syn" file exists.
    idxfilesize=   // required
    idxoffsetbits= // New in 3.0.0
    author=
    email=
    website=
    description=    // You can use <br> for new line.
    date=
    sametypesequence= // very important.
    """

    def __init__(self, dict_prefix, container):

        ifo_filename = '%s.ifo' % dict_prefix

        try:
            _file = open(ifo_filename, encoding="utf-8")
        except Exception as e:
            raise Exception('ifo file opening error: "{}"'.format(e))

        _file.readline()

        # skipping ifo header
        _line = _file.readline().split('=')
        if _line[0] == 'version':
            self.version = _line[1]
        else:
            raise Exception('ifo has invalid format')

        _config = {}
        for _line in _file:
            _line_splited = _line.split('=')
            _config[_line_splited[0]] = _line_splited[1]
        _file.close()

        self.bookname = _config.get('bookname', None).strip()
        if self.bookname is None:
            raise Exception('ifo has no bookname')

        self.wordcount = _config.get('wordcount', None)
        if self.wordcount is None:
            raise Exception('ifo has no wordcount')
        self.wordcount = int(self.wordcount)

        if self.version == '3.0.0':
            try:
                #_syn = open('%s.syn' % dict_prefix)    # not used
                self.synwordcount = _config.get('synwordcount', None)
                if self.synwordcount is None:
                    raise Exception(
                        'ifo has no synwordcount but .syn file exists')
                self.synwordcount = int(self.synwordcount)
            except IOError:
                pass

        self.idxfilesize = _config.get('idxfilesize', None)
        if self.idxfilesize is None:
            raise Exception('ifo has no idxfilesize')
        self.idxfilesize = int(self.idxfilesize)

        self.idxoffsetbits = _config.get('idxoffsetbits', 32)
        self.idxoffsetbits = int(self.idxoffsetbits)

        self.author = _config.get('author', '').strip()

        self.email = _config.get('email', '').strip()

        self.website = _config.get('website', '').strip()

        self.description = _config.get('description', '').strip()

        self.date = _config.get('date', '').strip()

        self.sametypesequence = _config.get('sametypesequence', '').strip()


class _StarDictIdx(object):
    """
    The .idx file is just a word list.

    The word list is a sorted list of word entries.

    Each entry in the word list contains three fields, one after the other:
         word_str;  // a utf-8 string terminated by '\0'.
         word_data_offset;  // word data's offset in .dict file
         word_data_size;  // word data's total size in .dict file 
    """

    def __init__(self, dict_prefix, container):
        self._container = container

        idx_filename = '%s.idx' % dict_prefix
        idx_filename_gz = '%s.gz' % idx_filename

        try:
            file = open_file(idx_filename, idx_filename_gz)
        except Exception as e:
            raise Exception('idx file opening error: "{}"'.format(e))

        self._file = file.read()

        """ check file size """
        if file.tell() != container.ifo.idxfilesize:
            raise Exception('size of the .idx file is incorrect')
        file.close()

        """ prepare main dict and parsing parameters """
        self._idx = {}
        idx_offset_bytes_size = int(container.ifo.idxoffsetbits / 8)
        idx_offset_format = {4: 'L', 8: 'Q', }[idx_offset_bytes_size]
        idx_cords_bytes_size = idx_offset_bytes_size + 4

        """ parse data via regex """
        record_pattern = br'([\d\D]+?\x00[\d\D]{' + str(
            idx_cords_bytes_size).encode('utf-8') + br'})'
        matched_records = re.findall(record_pattern, self._file)

        """ check records count """
        if len(matched_records) != container.ifo.wordcount:
            raise Exception('words count is incorrect')

        """ unpack parsed records """
        for matched_record in matched_records:
            c = matched_record.find(b'\x00')
            if c == 0:
                continue
            record_tuple = unpack(
                '!%sc%sL' % (c + 1, idx_offset_format), matched_record)
            word, cords = record_tuple[:c], record_tuple[c + 1:]
            self._idx[b''.join(word)] = cords

    def __getitem__(self, word):
        """
        returns tuple (word_data_offset, word_data_size,) for word in .dict

        @note: here may be placed flexible search realization
        """
        return self._idx[word.encode('utf-8')]

    def __contains__(self, k):
        """
        returns True if index has a word k, else False
        """
        return k.encode('utf-8') in self._idx

    def __eq__(self, y):
        """
        returns True if hashlib.md5(x.idx) is equal to hashlib.md5(y.idx), else False
        """
        return hashlib.md5(self._file).hexdigest() == hashlib.md5(y._file).hexdigest()

    def __ne__(self, y):
        """
        returns True if hashlib.md5(x.idx) is not equal to hashlib.md5(y.idx), else False
        """
        return not self.__eq__(y)

    def iterkeys(self):
        """
        returns iterkeys
        """
        if not self._container.in_memory:
            warnings.warn(
                'Iter dict items with in_memory=False may cause serious performance problem')
        for key in six.iterkeys(self._idx):
            yield key.decode('utf-8')

    def keys(self):
        """
        returns keys
        """
        if six.PY3:
            return self.iterkeys()

        if not self._container.in_memory:
            warnings.warn(
                'Iter dict items with in_memory=False may cause serious performance problem')
        return [key.decode('utf-8') for key in self._idx.keys()]


class _StarDictDict(object):
    """
    The .dict file is a pure data sequence, as the offset and size of each
    word is recorded in the corresponding .idx file.

    If the "sametypesequence" option is not used in the .ifo file, then
    the .dict file has fields in the following order:
    ==============
    word_1_data_1_type; // a single char identifying the data type
    word_1_data_1_data; // the data
    word_1_data_2_type;
    word_1_data_2_data;
    ...... // the number of data entries for each word is determined by
           // word_data_size in .idx file
    word_2_data_1_type;
    word_2_data_1_data;
    ......
    ==============
    It's important to note that each field in each word indicates its
    own length, as described below.  The number of possible fields per
    word is also not fixed, and is determined by simply reading data until
    you've read word_data_size bytes for that word.


    Suppose the "sametypesequence" option is used in the .idx file, and
    the option is set like this:
    sametypesequence=tm
    Then the .dict file will look like this:
    ==============
    word_1_data_1_data
    word_1_data_2_data
    word_2_data_1_data
    word_2_data_2_data
    ......
    ==============
    The first data entry for each word will have a terminating '\0', but
    the second entry will not have a terminating '\0'.  The omissions of
    the type chars and of the last field's size information are the
    optimizations required by the "sametypesequence" option described
    above.

    If "idxoffsetbits=64", the file size of the .dict file will be bigger 
    than 4G. Because we often need to mmap this large file, and there is 
    a 4G maximum virtual memory space limit in a process on the 32 bits 
    computer, which will make we can get error, so "idxoffsetbits=64" 
    dictionary can't be loaded in 32 bits machine in fact, StarDict will 
    simply print a warning in this case when loading. 64-bits computers 
    should haven't this limit.

    Type identifiers
    ----------------
    Here are the single-character type identifiers that may be used with
    the "sametypesequence" option in the .idx file, or may appear in the
    dict file itself if the "sametypesequence" option is not used.

    Lower-case characters signify that a field's size is determined by a
    terminating '\0', while upper-case characters indicate that the data
    begins with a network byte-ordered guint32 that gives the length of 
    the following data's size(NOT the whole size which is 4 bytes bigger).

    'm'
    Word's pure text meaning.
    The data should be a utf-8 string ending with '\0'.

    'l'
    Word's pure text meaning.
    The data is NOT a utf-8 string, but is instead a string in locale
    encoding, ending with '\0'.  Sometimes using this type will save disk
    space, but its use is discouraged.

    'g'
    A utf-8 string which is marked up with the Pango text markup language.
    For more information about this markup language, See the "Pango
    Reference Manual."
    You might have it installed locally at:
    file:///usr/share/gtk-doc/html/pango/PangoMarkupFormat.html

    't'
    English phonetic string.
    The data should be a utf-8 string ending with '\0'.

    Here are some utf-8 phonetic characters:
    θʃŋʧðʒæıʌʊɒɛəɑɜɔˌˈːˑṃṇḷ
    æɑɒʌәєŋvθðʃʒɚːɡˏˊˋ

    'x'
    A utf-8 string which is marked up with the xdxf language.
    See http://xdxf.sourceforge.net
    StarDict have these extention:
    <rref> can have "type" attribute, it can be "image", "sound", "video" 
    and "attach".
    <kref> can have "k" attribute.

    'y'
    Chinese YinBiao or Japanese KANA.
    The data should be a utf-8 string ending with '\0'.

    'k'
    KingSoft PowerWord's data. The data is a utf-8 string ending with '\0'.
    It is in XML format.

    'w'
    MediaWiki markup language.
    See http://meta.wikimedia.org/wiki/Help:Editing#The_wiki_markup

    'h'
    Html codes.

    'r'
    Resource file list.
    The content can be:
    img:pic/example.jpg     // Image file
    snd:apple.wav           // Sound file
    vdo:film.avi            // Video file
    att:file.bin            // Attachment file
    More than one line is supported as a list of available files.
    StarDict will find the files in the Resource Storage.
    The image will be shown, the sound file will have a play button.
    You can "save as" the attachment file and so on.

    'W'
    wav file.
    The data begins with a network byte-ordered guint32 to identify the wav
    file's size, immediately followed by the file's content.

    'P'
    Picture file.
    The data begins with a network byte-ordered guint32 to identify the picture
    file's size, immediately followed by the file's content.

    'X'
    this type identifier is reserved for experimental extensions.

    """

    def __init__(self, dict_prefix, container, in_memory=False):
        """
        opens regular or dziped .dict file

        'in_memory': indicate whether read whole dict file into memory
        """
        self._container = container
        self._in_memory = in_memory

        dict_filename = '%s.dict' % dict_prefix
        dict_filename_dz = '%s.dz' % dict_filename

        try:
            f = open_file(dict_filename, dict_filename_dz)
        except Exception as e:
            raise Exception('dict file opening error: "{}"'.format(e))

        if in_memory:
            self._file = f.read()
            f.close()
        else:
            self._file = f

    def __getitem__(self, word):
        """
        returns data from .dict for word
        """

        # getting word data coordinates
        cords = self._container.idx[word]

        if self._in_memory:
            bytes_ = self._file[cords[0]: cords[0] + cords[1]]
        else:
            # seeking in file for data
            self._file.seek(cords[0])

            # reading data
            bytes_ = self._file.read(cords[1])

        return bytes_.decode('utf-8')


class _StarDictSyn(object):

    def __init__(self, dict_prefix, container):

        syn_filename = '%s.syn' % dict_prefix

        try:
            self._file = open(syn_filename, encoding="utf-8")
        except IOError:
            # syn file is optional, passing silently
            pass


class Dictionary(dict):
    """
    Dictionary-like class for lazy manipulating stardict dictionaries

    All items of this dictionary are writable and dict is expandable itself,
    but changes are not stored anywhere and available in runtime only.

    We assume in this documentation that "x" or "y" is instances of the
    StarDictDict class and "x.{ifo,idx{,.gz},dict{,.dz},syn}" or
    "y.{ifo,idx{,.gz},dict{,.dz},syn}" is files of the corresponding stardict
    dictionaries.


    Following documentation is from the "dict" class an is subkect to rewrite
    in further impleneted methods:

    """

    def __init__(self, filename_prefix, in_memory=False):
        """
        filename_prefix: path to dictionary files without files extensions

        initializes new StarDictDict instance from stardict dictionary files
        provided by filename_prefix
        """

        self.in_memory = in_memory

        # reading somedict.ifo
        self.ifo = _StarDictIfo(dict_prefix=filename_prefix, container=self)

        # reading somedict.idx or somedict.idx.gz
        self.idx = _StarDictIdx(dict_prefix=filename_prefix, container=self)

        # reading somedict.dict or somedict.dict.dz
        self.dict = _StarDictDict(
            dict_prefix=filename_prefix, container=self, in_memory=in_memory)

        # reading somedict.syn (optional)
        self.syn = _StarDictSyn(dict_prefix=filename_prefix, container=self)

        # initializing cache
        self._dict_cache = {}

    def __cmp__(self, y):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def __contains__(self, k):
        """
        returns True if x.idx has a word k, else False
        """
        return k in self.idx

    def __delitem__(self, k):
        """
        frees cache from word k translation
        """
        del self._dict_cache[k]

    def __eq__(self, y):
        """
        returns True if hashlib.md5(x.idx) is equal to hashlib.md5(y.idx), else False
        """
        return self.idx.__eq__(y.idx)

    def __ge__(self, y):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def __getitem__(self, k):
        """
        returns translation for word k from cache or not and then caches
        """
        if k in self._dict_cache:
            return self._dict_cache[k]
        else:
            value = self.dict[k]
            self._dict_cache[k] = value
            return value

    def __gt__(self, y):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def __iter__(self):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def __le__(self):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def __len__(self):
        """
        returns number of words provided by wordcount parameter of the x.ifo
        """
        return self.ifo.wordcount

    def __lt__(self):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def __ne__(self, y):
        """
        returns True if hashlib.md5(x.idx) is not equal to hashlib.md5(y.idx), else False
        """
        return not self.__eq__(y)

    def __repr__(self):
        """
        returns classname and bookname parameter of the x.ifo
        """
        return u'%s %s' % (self.__class__, self.ifo.bookname)

    def __setitem__(self, k, v):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def clear(self):
        """
        clear dict cache 
        """
        self._dict_cache = dict()

    def get(self, k, d=''):
        """
        returns translation of the word k from self.dict or d if k not in x.idx

        d defaults to empty string
        """
        return k in self and self[k] or d

    def has_key(self, k):
        """
        returns True if self.idx has a word k, else False
        """
        return k in self

    def items(self):
        """
        returns items
        """
        if not self.in_memory:
            warnings.warn(
                'Iter dict items with in_memory=False may cause serious performance problem')
        return [(key, self[key]) for key in self.keys()]

    def iteritems(self):
        """
        returns iteritems
        """
        if not self.in_memory:
            warnings.warn(
                'Iter dict items with in_memory=False may cause serious performance problem')
        for key in self.iterkeys():
            yield (key, self[key])

    def iterkeys(self):
        """
        returns iterkeys
        """
        if not self.in_memory:
            warnings.warn(
                'Iter dict items with in_memory=False may cause serious performance problem')
        return self.idx.iterkeys()

    def itervalues(self):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def keys(self):
        """
        returns keys
        """
        if not self.in_memory:
            warnings.warn(
                'Iter dict items with in_memory=False may cause serious performance problem')
        return self.idx.keys()

    def pop(self, k, d):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def popitem(self):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def setdefault(self, k, d):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def update(self, E, **F):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def values(self):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()

    def fromkeys(self, S, v=None):
        """
        raises NotImplemented exception
        """
        raise NotImplementedError()


def open_file(regular, gz):
    """
    Open regular file if it exists, gz file otherwise.
    If no file exists, raise ValueError.
    """
    if os.path.exists(regular):
        try:
            return open(regular, 'rb')
        except Exception as e:
            raise Exception('regular file opening error: "{}"'.format(e))

    if os.path.exists(gz):
        try:
            return gzip.open(gz, 'rb')
        except Exception as e:
            raise Exception('gz file opening error: "{}"'.format(e))

    raise ValueError('Neither regular nor gz file exists')
