import time

from core.handler import Handler
from core.utils import *


class SemanticTokens(Handler):
    name = "semantic_tokens"
    method = "textDocument/semanticTokens/full"
    cancel_on_change = True
    send_document_uri = True

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.tokens = None
        self.render_tokens = set()
        self.buffer_name = None
        self.pre_last_change = (time.time(), time.time())
        self.type_face_names = None
        self.type_face_names_dict = dict()
        self.type_modifier_face_names = None
        self.ignore_modifier_limit_types = None

    def process_request(self, buffer_name, force):
        self.buffer_name = buffer_name
        self.cancel_send_request = False

        if (self.file_action.single_server is None or
            self.file_action.single_server.semantic_tokens_provider is None):
            # server not support
            self.cancel_send_request = True
            return None
        if force:
            return dict()

        if self.pre_last_change[0] == self.last_change[0] and self.tokens is not None:
            self.cancel_send_request = True
            self.update_tokens(self.tokens) # When the file has not changed, use cached tokens
        else:
            self.pre_last_change = self.last_change
            return dict()

    def process_response(self, response):
        if response is None:
            return
        data = response.get("data")
        if data is None:
            return
        self.tokens = data
        self.update_tokens(self.tokens)

    def update_tokens(self, tokens):
        if tokens is None:
            return

        window_line_range = get_emacs_func_result("get-window-line-range", self.buffer_name)
        if window_line_range is None or len(window_line_range) != 2:
            return
        [window_line_begin, window_line_end] = window_line_range

        # window line start at 1, token line start at 0
        window_line_begin -= 1
        window_line_end -= 1

        index = 0
        cur_line = 0
        column = 0
        render_tokens = set()
        while index < len(tokens):
            if tokens[index] != 0:
                column = 0
                cur_line += tokens[index]
            column += tokens[index + 1]
            if cur_line >= window_line_begin and cur_line <= window_line_end: # in window range
                faces_index = self.get_faces_index(tokens[index + 3], tokens[index + 4])
                if faces_index is not None:
                    render_tokens.add((cur_line, column, tokens[index + 2], faces_index[0], faces_index[1]))
            index = index + 5

        (new_tokens, old_tokens) = self.calc_diff_tokens(self.render_tokens, render_tokens)
        if len(new_tokens) != 0:
            eval_in_emacs("lsp-bridge-semantic-tokens--update", self.buffer_name, list(old_tokens), self.absolute_line_to_relative(new_tokens))
        self.render_tokens = render_tokens

    def get_faces_index(self, type_index, type_modifier_index):
        type_name = self.file_action.single_server.semantic_tokens_provider["legend"]["tokenTypes"][type_index]
        ignore_modifier = self.is_ignore_modifier(type_name)
        if type_modifier_index == 0 and not ignore_modifier:
            return None
        type_face_index = self.get_type_face_index(type_name)
        if type_face_index is None:
            return None
        type_modifier_faces_index = self.get_type_modifier_faces_index(type_modifier_index)
        if len(type_modifier_faces_index) == 0 and not ignore_modifier:
            return None
        return (type_face_index, type_modifier_faces_index)

    def get_type_face_index(self, type_name):
        if self.type_face_names is None:
            type_faces = get_emacs_var("lsp-bridge-semantic-tokens-type-faces")
            if type_faces is not None:
                faces = type_faces.value()
                self.type_face_names = dict()
                for index in range(len(faces)):
                    self.type_face_names[faces[index][0]] = index
            else:
                return None
        face_index = self.type_face_names.get(type_name)
        return face_index

    def get_type_modifier_faces_index(self, type_modifier_index):
        if self.type_modifier_face_names is None:
            type_modifier_faces = get_emacs_var("lsp-bridge-semantic-tokens-type-modifier-faces")
            if type_modifier_faces is not None:
                faces = type_modifier_faces.value()
                self.type_modifier_face_names = dict()
                for index in range(len(faces)):
                    self.type_modifier_face_names[faces[index][0]] = index
            else:
                return ()

        token_modifiers = self.file_action.single_server.semantic_tokens_provider["legend"]["tokenModifiers"]
        type_modifier_names = [token_modifiers[index] for index in self.find_ones(type_modifier_index)]
        type_modifier_faces_index = []
        for name in type_modifier_names:
            index = self.type_modifier_face_names.get(name)
            if index is not None:
                type_modifier_faces_index.append(index)
        return tuple(type_modifier_faces_index)

    def is_ignore_modifier(self, type_name):
        if self.ignore_modifier_limit_types is None:
            types = get_emacs_var("lsp-bridge-semantic-tokens-ignore-modifier-limit-types").value()
            if types is not None:
                self.ignore_modifier_limit_types = dict()
                for index in range(len(types)):
                    self.ignore_modifier_limit_types[types[index]] = index

        return type_name in self.ignore_modifier_limit_types

    def calc_diff_tokens(self, pre_tokens, cur_tokens):
        common_tokens = cur_tokens & pre_tokens
        if len(common_tokens) == len(cur_tokens):
            return ([], [])
        new_tokens = list(cur_tokens - common_tokens)
        old_tokens = list(pre_tokens - common_tokens)
        new_tokens.sort(key = lambda token : token[0])
        old_tokens.sort(key = lambda token : token[0])
        return (new_tokens, old_tokens)

    def absolute_line_to_relative(self, tokens):
        relative_tokens = []
        cur_line = 0
        delta_line = 0
        for token in tokens:
            delta_line = token[0] - cur_line
            if token[0] != cur_line:
                cur_line = token[0]
            relative_tokens.append((delta_line, token[1], token[2], token[3], token[4]))

        return relative_tokens

    def find_ones(self, num):
        ones = []
        bit = 0
        while num > 0:
            if num & 1:
                ones.append(bit)
            num >>= 1
            bit += 1
        return ones
