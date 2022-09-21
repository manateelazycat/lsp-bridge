import platform
import stat
import os
from urllib.request import urlopen, urlretrieve
from urllib.error import HTTPError
import zipfile
import threading

_TABNINE_SERVER_URL = "https://update.tabnine.com/bundles"
_TABNINE_EXECUTABLE = "TabNine"

arch_translations = {
    "arm64": "aarch64",
    "AMD64": "x86_64",
}


class TabnineDownloader(threading.Thread):
    def __init__(self, download_url, output_dir):
        threading.Thread.__init__(self)
        self.download_url = download_url
        self.output_dir = output_dir

    def run(self):
        try:
            if not os.path.isdir(self.output_dir):
                os.makedirs(self.output_dir)
            zip_path, _ = urlretrieve(self.download_url)
            with zipfile.ZipFile(zip_path, "r") as zf:
                for filename in zf.namelist():
                    zf.extract(filename, self.output_dir)
                    target = os.path.join(self.output_dir, filename)
                    add_execute_permission(target)
        except Exception as e:
            return


def install_tabnine_at(binary_dir):
    version = get_tabnine_version()
    distro = get_distribution_name()
    download_url = "{}/{}/{}/{}.zip".format(
        _TABNINE_SERVER_URL, version, distro, _TABNINE_EXECUTABLE
    )
    output_dir = os.path.join(binary_dir, version, distro)
    TabnineDownloader(download_url, output_dir).start()


def get_tabnine_path(binary_dir):
    distro = get_distribution_name()
    versions = os.listdir(binary_dir)
    versions.sort(key=parse_semver, reverse=True)
    for version in versions:
        path = os.path.join(
            binary_dir, version, distro, executable_name(_TABNINE_EXECUTABLE)
        )
        if os.path.isfile(path):
            return path


def get_distribution_name():
    sysinfo = platform.uname()
    sys_architecture = sysinfo.machine

    if sys_architecture in arch_translations:
        sys_architecture = arch_translations[sys_architecture]

    if sysinfo.system == "Windows":
        sys_platform = "pc-windows-gnu"

    elif sysinfo.system == "Darwin":
        sys_platform = "apple-darwin"

    elif sysinfo.system == "Linux":
        sys_platform = "unknown-linux-musl"

    elif sysinfo.system == "FreeBSD":
        sys_platform = "unknown-freebsd"

    else:
        raise RuntimeError(
            "Platform was not recognized as any of " "Windows, macOS, Linux, FreeBSD"
        )

    return "{}-{}".format(sys_architecture, sys_platform)


def parse_semver(s):
    try:
        return [int(x) for x in s.split(".")]
    except ValueError:
        return []


def executable_name(name):
    if platform.system() == "Windows":
        return name + ".exe"
    else:
        return name


def add_execute_permission(path):
    st = os.stat(path)
    new_mode = st.st_mode | stat.S_IEXEC
    if new_mode != st.st_mode:
        os.chmod(path, new_mode)


def get_tabnine_version():
    version_url = "{}/{}".format(_TABNINE_SERVER_URL, "version")

    try:
        return urlopen(version_url).read().decode("UTF-8").strip()
    except HTTPError:
        return None
