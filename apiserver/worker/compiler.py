# compiler.py
# Original Author: Jeff Cameron (jeff@jpcameron.com)

import collections
import errno
import fnmatch
import os
import os.path
import re
import subprocess
import sys

import util

try:
    from server_info import server_info
    MEMORY_LIMIT = server_info.get('memory_limit', 1500)
except ImportError:
    MEMORY_LIMIT = 1500

BOT = "MyBot"
# Which file is used to override the detected language?
LANGUAGE_FILE = "LANGUAGE"
SAFEPATH = re.compile('[a-zA-Z0-9_.$-]+$')


class CD(object):
    """
    A context manager that enters a given directory and restores the old one.
    """
    def __init__(self, new_dir):
        self.new_dir = new_dir

    def __enter__(self):
        self.org_dir = os.getcwd()
        os.chdir(self.new_dir)
        return self.new_dir

    def __exit__(self, type, value, traceback):
        os.chdir(self.org_dir)


def safeglob(pattern):
    safepaths = []
    for root, dirs, files in os.walk("."):
        files = fnmatch.filter(files, pattern)
        for fname in files:
            if SAFEPATH.match(fname) and os.path.splitext(fname)[0] != "RandomBot":
                safepaths.append(os.path.join(root, fname))
    return safepaths


def safeglob_multi(patterns):
    safepaths = []
    for pattern in patterns:
        safepaths.extend(safeglob(pattern))
    return safepaths


def nukeglob(pattern):
    paths = safeglob(pattern)
    for path in paths:
        try:
            os.unlink(path)
        except OSError as e:
            if e.errno != errno.ENOENT and e.errno != errno.EACCES:
                raise


def _run_cmd(cmd, working_dir, timelimit):
    """
    Run a compilation command in the sandbox.
    :param cmd: The command to run.
    :param working_dir: The directory to run the command in.
    :param timelimit: The number of seconds the command is allowed to run.
    :return: The value of stdout as well as any errors that occurred.
    """
    absoluteWorkingDir = os.path.abspath(working_dir)
    cmd = "sudo -H -iu bot_compilation bash -c \"cd "+absoluteWorkingDir+"; "+cmd+"\""
    print(cmd)
    process = subprocess.Popen(cmd, cwd=working_dir, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE)

    try:
        rawOut, rawErrors = process.communicate(timeout=timelimit)
        outString = rawOut.decode("utf-8").strip()
        out = outString.split("\n") if outString.isspace() == False and outString != "" else None

        errorsString = rawErrors.decode("utf-8").strip()
        errors = errorsString.split("\n") if errorsString.isspace() == False and errorsString != "" else None
    except subprocess.TimeoutExpired:
        out = []
        errors = ["Compilation timed out with command %s" % (cmd,)]

    # Clean up any processes that didn't exit cleanly
    util.kill_processes_as("bot_compilation")

    return out, errors


def check_path(path, errors):
    print(path)
    if not os.path.exists(path):
        errors.append("Output file " + str(os.path.basename(path)) + " was not created.")
        return False
    else:
        return True


class Compiler(object):
    """
    A way to compile an uploaded bot.
    """
    def compile(self, bot_dir, globs, errors, timelimit):
        raise NotImplementedError


class ChmodCompiler(Compiler):
    """
    A compiler that simply sets the executable flag on a file.
    """
    def __init__(self, language):
        self.language = language

    def __str__(self):
        return "ChmodCompiler: %s" % (self.language,)

    def compile(self, bot_dir, globs, errors, timelimit):
        with CD(bot_dir):
            for f in safeglob_multi(globs):
                try:
                    os.chmod(f, 0o755)
                except Exception as e:
                    errors.append("Error chmoding %s - %s\n" % (f, e))
        return True


class CSharpMonoCompiler(Compiler):
    def compile(self, bot_dir, globs, errors, timelimit):
        with CD(bot_dir):
            print("Looking for *.cs files:")
            cs_files = safeglob("*.cs")
            print(str(len(cs_files)) + " found")
            if len(cs_files) == 0:
                print("No *.cs files found, nothing to do, exiting successfully.")
                return True

            print("Looking for *.dll files:")
            dll_files = safeglob("*.dll")
            print(str(len(dll_files)) + " found")

        try:
            cmd_args = ["mcs", "-warn:0", "-optimize+", "-pkg:dotnet", "-out:MyBot.exe"]
            if len(dll_files) != 0:
                cmd_args.append("-r:" + ",".join(dll_files))
            cmd_args += cs_files

            cmdline = " ".join(cmd_args)
            cmd_out, cmd_errors = _run_cmd(cmdline, bot_dir, timelimit)
            if not cmd_errors:
                check_path(os.path.join(bot_dir, "MyBot.exe"), cmd_errors)
                if cmd_errors:
                    cmd_errors += cmd_out
            if cmd_errors:
                errors += cmd_errors
                return False
        except:
            pass
        return True


class ExternalCompiler(Compiler):
    """
    A compiler that calls an external process.
    """
    def __init__(self, args, separate=False, out_files=[], out_ext=None):
        self.args = args
        self.separate = separate
        self.out_files = out_files
        self.out_ext = out_ext
        self.stderr_re = re.compile("WARNING: IPv4 forwarding is disabled")

    def __str__(self):
        return "ExternalCompiler: %s" % (' '.join(self.args),)

    def compile(self, bot_dir, globs, errors, timelimit):
        with CD(bot_dir):
            print("GLOBS: " + ", ".join(globs))
            files = safeglob_multi(globs)
            if len("".join(globs)) != 0 and len(files) == 0:
                # no files to compile
                return True

        try:
            if self.separate:
                for filename in files:
                    print("file: " + filename)
                    cmdline = " ".join(self.args + [filename])
                    cmd_out, cmd_errors = _run_cmd(cmdline, bot_dir, timelimit)
                    cmd_errors = self.cmd_error_filter(cmd_out, cmd_errors)
                    if not cmd_errors:
                        for ofile in self.out_files:
                            check_path(os.path.join(bot_dir, ofile), cmd_errors)
                        if self.out_ext:
                            oname = os.path.splitext(filename)[0] + self.out_ext
                            check_path(os.path.join(bot_dir, oname), cmd_errors)
                        if cmd_errors:
                            cmd_errors += cmd_out
                    if cmd_errors:
                        errors += cmd_errors
                        return False
            else:
                cmdline = " ".join(self.args + files)
                print("Files: " + " ".join(files))
                cmd_out, cmd_errors = _run_cmd(cmdline, bot_dir, timelimit)
                cmd_errors = self.cmd_error_filter(cmd_out, cmd_errors)
                if not cmd_errors:
                    for ofile in self.out_files:
                        check_path(os.path.join(bot_dir, ofile), cmd_errors)
                    if self.out_ext:
                        for filename in files:
                            oname = os.path.splitext(filename)[0] + self.out_ext
                            check_path(os.path.join(bot_dir, oname), cmd_errors)
                    if cmd_errors:
                        cmd_errors += cmd_out
                if cmd_errors:
                    errors += cmd_errors
                    return False
        except:
            pass
        return True

    def cmd_error_filter(self, cmd_out, cmd_errors):
        cmd_errors = [line for line in cmd_errors if line is None or self.stderr_re.search(line) is None]
        return cmd_errors


class ErrorFilterCompiler(ExternalCompiler):
    def __init__(self, args, separate=False, out_files=[], out_ext=None, stdout_is_error=False, skip_stdout=0, filter_stdout=None, filter_stderr=None):
        ExternalCompiler.__init__(self, args, separate, out_files, out_ext)
        self.stdout_is_error = stdout_is_error
        self.skip_stdout = skip_stdout;
        if filter_stdout is None:
            self.stdout_re = None
        else:
            self.stdout_re = re.compile(filter_stdout)
        if filter_stderr is None:
            self.stderr_re = None
        else:
            self.stderr_re = re.compile(filter_stderr)

    def __str__(self):
        return "ErrorFilterCompiler: %s" % (' '.join(self.args),)

    def cmd_error_filter(self, cmd_out, cmd_errors):
        cmd_errors = ExternalCompiler.cmd_error_filter(self, cmd_out, cmd_errors)

        if self.skip_stdout > 0:
            del cmd_out[:self.skip_stdout]
        # Somehow there are None values in the output
        if self.stdout_re is not None:
            cmd_out = [line for line in cmd_out if
                       line is None or not self.stdout_re.search(line)]
        if self.stderr_re is not None:
            cmd_errors = [line for line in cmd_errors if
                          line is None or self.stderr_re.search(line) is None]
        if self.stdout_is_error:
            return [line for line in cmd_out if line is not None] + cmd_errors

        return cmd_errors


class TargetCompiler(Compiler):
    def __init__(self, args, replacements, outflag="-o"):
        self.args = args
        self.replacements = replacements
        self.outflag = outflag

    def __str__(self):
        return "TargetCompiler: %s" % (' '.join(self.args),)

    def compile(self, bot_dir, globs, errors, timelimit):
        with CD(bot_dir):
            sources = safeglob_multi(globs)

        try:
            for source in sources:
                head, ext = os.path.splitext(source)
                if ext in self.replacements:
                    target = head + self.replacements[ext]
                else:
                    errors.append("Could not determine target for source file %s." % source)
                    return False
                cmdline = " ".join(self.args + [self.outflag, target, source])
                cmd_out, cmd_errors = _run_cmd(cmdline, bot_dir, timelimit)
                if cmd_errors:
                    errors += cmd_errors
                    return False
        except:
            pass
        return True

PYTHON_EXT_COMPILER = '''"from distutils.core import setup
from distutils.extension import read_setup_file
setup(ext_modules = read_setup_file('setup_exts'), script_args = ['-q', 'build_ext', '-i'])"'''

# Arrays of compiler commands to run for each language.
comp_args = {
    "Ada": [
        ["gcc-4.4", "-O3", "-funroll-loops", "-c"],
        ["gnatbind"],
        ["gnatlink", "-o", BOT],
    ],
    "CMake": [
        ["cmake", "."],
        ["make", "-j2"],
    ],
    "C": [
        ["gcc", "-O3", "-funroll-loops", "-c"],
        ["gcc", "-O2", "-lm", "-o", BOT],
    ],
    "C#/.NET Core": [
        ["dotnet", "restore"],
        ["dotnet", "build", "-c", "Release", "-o", "."],
    ],
    "Clojure": [
        ["lein", "uberjar"],
    ],
    "VB/Mono": [
        ["vbnc", "-out:%s.exe" % BOT],
    ],
    "C++": [
        ["g++", "-O3", "-w", "-std=c++11", "-c"],
        ["g++", "-O2", "-lm", "-std=c++11", "-o", BOT],
    ],
    "D": [
        ["dmd", "-O", "-inline", "-release", "-noboundscheck", "-of" + BOT],
    ],
    "Elixir": [
        ["yes", "|", "mix", "deps.get"],
        ["mix", "escript.build"],
    ],
    "Groovy": [
        ["groovyc"],
        ["jar", "cfe", BOT + ".jar", BOT],
    ],
    "Julia": [
        ["JULIA_PKGDIR=$(pwd) julia -e 'Pkg.init(); Pkg.update()'"],
        ["mv", "REQUIRE", "v0.6/"],
        ["JULIA_PKGDIR=$(pwd) julia -e 'Pkg.resolve()'"],
        ["chmod", "+x", BOT + ".jl"],
    ],
    "Haskell": [
        ["ghc", "--make", BOT + ".hs", "-O", "-v0", "-rtsopts"],
    ],
    "Haskell/Stack": [
        ["stack", "build", "--allow-different-user"],
        ["stack", "install", "--local-bin-path", "."],
    ],
    "Java": [
        ["javac", "-encoding", "UTF-8", "-J-Xmx%sm" % (MEMORY_LIMIT)],
    ],
    "Lisp": [
        ['sbcl', '--dynamic-space-size', str(MEMORY_LIMIT), '--script', BOT + '.lisp'],
    ],
    "OCaml": [
        ["ocamlbuild -lib unix", BOT + ".native"],
    ],
    "Pascal": [
        ["fpc", "-Mdelphi", "-Si", "-O3", "-Xs", "-v0", "-o" + BOT],
    ],
    "Python": [
        ["python3.6", "-c", PYTHON_EXT_COMPILER],
    ],
    "Rust": [
        ["cargo", "rustc", "--release", "-q", "--", "-Awarnings"],
    ],
    "Scala": [
        ["scalac"],
    ],
    "Swift": [
        ["swift", "build", "--configuration", "release"],
        ["chmod", "-R", "777", ".build"],
        ["mv", ".build/release/MyBot", "./MyBot"],
    ],
}

targets = {
    "C"  : { ".c" : ".o" },
    "C++": { ".c" : ".o", ".cpp" : ".o", ".cc" : ".o" },
    }

Language = collections.namedtuple(
    "Language",
    ['name', 'out_file', 'main_code_file', 'command',
     'nukeglobs', 'compilers'])

# The actual languages supported.
languages = (
    Language("Ada", BOT, "MyBot.adb",
        "./MyBot",
        ["*.ali"],
        [(["*.adb"], ExternalCompiler(comp_args["Ada"][0])),
            (["MyBot.ali"], ExternalCompiler(comp_args["Ada"][1])),
            (["MyBot.ali"], ExternalCompiler(comp_args["Ada"][2]))]
    ),
    Language("C++", BOT, "CMakeLists.txt",
        "./MyBot",
        ["*.o", BOT],
        [
            ([], ExternalCompiler(comp_args["CMake"][0])),
            ([], ExternalCompiler(comp_args["CMake"][1])),
        ]
    ),
    Language("C", BOT, "MyBot.c",
        "./MyBot",
        ["*.o", BOT],
        [(["*.c"], TargetCompiler(comp_args["C"][0], targets["C"])),
            (["*.o"], ExternalCompiler(comp_args["C"][1]))]
    ),
    Language("C#/.NET Core", BOT + ".dll", "MyBot.csproj",
             "dotnet MyBot.dll",
             [BOT + ".dll"],
             [
                 ([], ExternalCompiler(comp_args["C#/.NET Core"][0])),
                 ([], ExternalCompiler(comp_args["C#/.NET Core"][1])),
             ]
    ),
    Language("C#/Mono", "MyBot.exe", "MyBot.cs",
        "mono MyBot.exe",
        ["MyBot.exe"],
        [([], CSharpMonoCompiler())]
    ),
    Language("VB/Mono", BOT +".exe", "MyBot.vb",
        "mono MyBot.exe",
        [BOT + ".exe"],
        [(["*.vb"],
            ExternalCompiler(comp_args["VB/Mono"][0], out_files=['MyBot.exe']))]
    ),
    Language("C++", BOT, "MyBot.cpp",
        "./MyBot",
        ["*.o", BOT],
        [
            (["*.c", "*.cpp", "*.cc"],
                TargetCompiler(comp_args["C++"][0], targets["C++"])),
            (["*.o"], ExternalCompiler(comp_args["C++"][1]))
        ]
    ),
    Language("Clojure", "target/"+BOT +".jar", "project.clj",
        "java -cp target/MyBot.jar MyBot",
        [],
        [([""], ErrorFilterCompiler(comp_args["Clojure"][0], filter_stderr="(Retrieving|Compiling)"))]
    ),
    Language("CoffeeScript", BOT +".coffee", "MyBot.coffee",
        "coffee MyBot.coffee",
        [],
        [(["*.coffee"], ChmodCompiler("CoffeeScript"))]
    ),
    Language("D", BOT, "MyBot.d",
        "./MyBot",
        ["*.o", BOT],
        [(["*.d"], ExternalCompiler(comp_args["D"][0]))]
    ),
    Language("Dart", BOT +".dart", "MyBot.dart",
        "dart MyBot.dart",
        [], [(["*.dart"], ChmodCompiler("Dart"))]
    ),
    Language("Elixir", BOT, "mix.exs",
        "./MyBot",
        [],
        [
            ([], ErrorFilterCompiler(comp_args["Elixir"][0])),
            ([], ErrorFilterCompiler(comp_args["Elixir"][1])),
        ]
    ),
    Language("Erlang", "my_bot.beam", "my_bot.erl",
        "erl -hms"+ str(MEMORY_LIMIT) +"m -smp disable -noshell -s my_bot start -s init stop",
        ["*.beam"],
        [(["*.erl"], ExternalCompiler(["erlc"], out_ext=".beam"))]
    ),
    Language("Go", BOT +".go", "MyBot.go",
        "export GOPATH=\"$(pwd)\"; go run MyBot.go",
        [],
        [(["*.go"], ChmodCompiler("Go"))]
    ),
    Language("Groovy", BOT +".jar", "MyBot.groovy",
        "java -Xmx" + str(MEMORY_LIMIT) + "m -cp MyBot.jar:/usr/share/groovy/embeddable/groovy-all-1.7.5.jar MyBot",
        ["*.class, *.jar"],
        [(["*.groovy"], ExternalCompiler(comp_args["Groovy"][0])),
        (["*.class"], ExternalCompiler(comp_args["Groovy"][1]))]
    ),
    Language("Haskell", BOT, "MyBot.hs",
        "./MyBot +RTS -M" + str(MEMORY_LIMIT) + "m",
        [BOT],
        [([""], ExternalCompiler(comp_args["Haskell"][0]))]
    ),
    Language("Haskell/Stack", BOT, "stack.yaml",
        "./MyBot +RTS -M" + str(MEMORY_LIMIT) + "m",
        [BOT],
        [
            ([""], ErrorFilterCompiler(comp_args["Haskell/Stack"][0], filter_stderr="error:")),
            ([""], ErrorFilterCompiler(comp_args["Haskell/Stack"][1], filter_stderr="error:", skip_stdout=1)),
        ]
    ),
    Language("Java", BOT +".java", "MyBot.java",
        "java MyBot",
        ["*.class", "*.jar"],
        [(["*.java"], ErrorFilterCompiler(comp_args["Java"][0], filter_stderr="Note:", out_files=["MyBot.class"]))]
    ),
    Language("JavaScript", BOT +".js", "MyBot.js",
        "node MyBot.js",
        [],
        [(["*.js"], ChmodCompiler("JavaScript"))]
    ),
    Language("JAR", BOT +".jar", "MyBot.jar",
        "java -jar MyBot.jar",
        [],
        [(["*.jar"], ChmodCompiler("JAR"))]
    ),
    Language("Julia", BOT +".jl", "MyBot.jl",
        "JULIA_PKGDIR=$(pwd) julia MyBot.jl",
        [],
        [
            ([], ErrorFilterCompiler(comp_args["Julia"][0])),
            ([], ErrorFilterCompiler(comp_args["Julia"][1])),
            ([], ErrorFilterCompiler(comp_args["Julia"][2])),
            ([], ErrorFilterCompiler(comp_args["Julia"][3])),
        ]
    ),
    Language("Lisp", BOT, "MyBot.lisp",
        "./MyBot --dynamic-space-size " + str(MEMORY_LIMIT),
        [BOT],
        [([""], ExternalCompiler(comp_args["Lisp"][0]))]
    ),
    Language("Lua/LuaJIT", BOT +".lua", "MyBot.lua",
        "luajit MyBot.lua",
        [],
        [(["*.lua"], ChmodCompiler("Lua"))]
    ),
    # Language("Lua/Lua5.2", BOT +".lua", "MyBot.lua",
    #     "lua5.2 MyBot.lua",
    #     [],
    #     [(["*.lua"], ChmodCompiler("Lua"))]
    # ),
    Language("OCaml", BOT +".native", "MyBot.ml",
        "./MyBot.native",
        [BOT + ".native"],
        [([""], ExternalCompiler(comp_args["OCaml"][0]))]
    ),
    Language("Octave", BOT + ".m", "MyBot.m",
        "octave -qf MyBot.m",
        [],
        [(["*.m"], ChmodCompiler("Octave"))]
    ),
    Language("Pascal", BOT, BOT + ".pas",
        "./" + BOT,
        [BOT, "*.o", "*.ppu"],
        [([BOT + ".pas"], ErrorFilterCompiler(comp_args["Pascal"][0],
           stdout_is_error=True, skip_stdout=2,
           filter_stderr='^/usr/bin/ld: warning: link.res contains output sections; did you forget -T\?$'))]
    ),
    Language("Perl", BOT +".pl", "MyBot.pl",
        "perl MyBot.pl",
        [],
        [(["*.pl"], ChmodCompiler("Perl"))]
    ),
    Language("PHP", BOT +".php", "MyBot.php",
        "php MyBot.php",
        [],
        [(["*.php"], ChmodCompiler("PHP"))]
    ),
    Language("Python", BOT +".py", "MyBot.py",
        "python3.6 MyBot.py",
        ["*.pyc"],
        [(["*.py"], ChmodCompiler("Python")),
        (["setup_exts"], ErrorFilterCompiler(comp_args["Python"][0], separate=True, filter_stderr='-Wstrict-prototypes'))]
    ),
    Language("Racket", BOT +".rkt", "MyBot.rkt",
        "racket MyBot.rkt",
        [],
        [(["*.rkt"], ChmodCompiler("Racket"))]
    ),
    Language("Ruby", BOT +".rb", "MyBot.rb",
        "ruby MyBot.rb",
        [],
        [(["*.rb"], ChmodCompiler("Ruby"))]
    ),
    Language("Rust", "target/release/"+BOT, "Cargo.toml",
        "target/release/MyBot",
        [],
        [([""], ErrorFilterCompiler(comp_args["Rust"][0], filter_stderr="warning:"))]
    ),
    Language("Scala", BOT +".scala", "MyBot.scala",
        'scala -J-Xmx'+ str(MEMORY_LIMIT) +'m -howtorun:object MyBot',
        ["*.scala, *.jar"],
        [(["*.java"], ExternalCompiler(comp_args["Java"][0])), (["*.scala"], ExternalCompiler(comp_args["Scala"][0]))]
    ),
    Language("Scheme", BOT +".ss", "MyBot.ss",
        "./MyBot",
        [],
        [(["*.ss"], ChmodCompiler("Scheme"))]
    ),
    Language("Swift", BOT, "Package.swift",
        "./MyBot",
        [],
        [
            ([""], ErrorFilterCompiler(comp_args["Swift"][0], filter_stderr="warning:")),
            ([""], ExternalCompiler(comp_args["Swift"][1])),
            ([""], ExternalCompiler(comp_args["Swift"][2])),
        ]
    ),
    Language("Tcl", BOT +".tcl", "MyBot.tcl",
        "tclsh8.5 MyBot.tcl",
        [],
        [(["*.tcl"], ChmodCompiler("Tcl"))]
    ),
)


def compile_function(language, bot_dir, timelimit):
    with CD(bot_dir):
        print("cd " + bot_dir)
        for glob in language.nukeglobs:
            print("nuke " + glob)
            nukeglob(glob)

    errors = []
    for globs, compiler in language.compilers:
        try:
            if not compiler.compile(bot_dir, globs, errors, timelimit):
                return False, errors
        except Exception as exc:
            raise
            errors.append("Compiler %s failed with: %s"
                    % (compiler, exc))
            return False, errors
    print("joining")
    compiled_bot_file = os.path.join(bot_dir, language.out_file)
    print("joined")
    return check_path(compiled_bot_file, errors), errors

_LANG_NOT_FOUND = """Did not find a recognized MyBot.* file.
Please add one of the following filenames to your zip file:
%s"""

def detect_language(bot_dir):
    with CD(bot_dir):
        detected_langs = [
            lang for lang in languages if os.path.exists(lang.main_code_file)
        ]

        # if we have cmake-based compilation, then remove any other autodetected C and C++ compilers
        if any(lang.main_code_file == "CMakeLists.txt" for lang in detected_langs):
            detected_langs = [lang for lang in detected_langs if
                              lang.main_code_file == "CMakeLists.txt" or (lang.name != "C" and lang.name != "C++")]

        if len(detected_langs) > 1:
            return None, ['Found multiple MyBot.* files: \n'+
                          '\n'.join([l.main_code_file for l in detected_langs])]
        elif len(detected_langs) == 0:
            return None, [_LANG_NOT_FOUND % (
                '\n'.join(l.name +": "+ l.main_code_file for l in languages),)]
        else:
            return detected_langs[0], None

def detect_language_file(bot_dir):
    with CD(bot_dir):
        try:
            with open(LANGUAGE_FILE, 'r') as lang_file:
                print("detected %s file" % LANGUAGE_FILE)
                language_name = lang_file.readline().strip()

                if not language_name:
                    return None
                else:
                    return language_name
        except IOError:
            return None

def get_run_cmd(submission_dir):
    with CD(submission_dir):
        if os.path.exists('run.sh'):
            with open('run.sh') as f:
                for line in f:
                    if line[0] != '#':
                        return line.rstrip('\r\n')

def get_run_lang(submission_dir):
    with CD(submission_dir):
        if os.path.exists('run.sh'):
            with open('run.sh') as f:
                for line in f:
                    if line[0] == '#':
                        return line[1:-1]


INSTALL_ERROR_START = "Possible errors running install.sh. stdout output as follows:"
INSTALL_ERROR_MID = "End of install.sh stdout. stderr as follows:"
INSTALL_ERROR_END = "End of install.sh output."


def truncate_errors(install_stdout, install_errors, language_detection_errors,
                    compile_errors, max_error_len=10*1024):
    """
    Combine lists of errors into a single list under a maximum length.
    """
    install_stdout = install_stdout or []
    install_errors = install_errors or []
    language_detection_errors = language_detection_errors or []
    compile_errors = compile_errors or []

    all_errors = install_stdout + install_errors + language_detection_errors + compile_errors
    result = []

    if sum(len(line) for line in all_errors) <= max_error_len:
        if install_stdout or install_errors:
            result.append(INSTALL_ERROR_START)
            result.extend(install_stdout)
            result.append(INSTALL_ERROR_MID)
            result.extend(install_errors)
            result.append(INSTALL_ERROR_END)
        result.extend(language_detection_errors)
        result.extend(compile_errors)
        return result

    def bound_errors(source, bound):
        total_length = sum(len(line) for line in source)
        if total_length <= bound:
            return total_length, source

        length = 0
        current = 0
        result = []
        # Take 1/3 from start of errors
        while current < len(source) and (
                length == 0 or
                length + len(source[current]) < bound // 3):
            result.append(source[current])
            length += len(source[current])
            current += 1

        if current < len(source):
            result.append("...(output truncated)...")

            end_errors = []
            end = current
            current = -1
            while current >= -(len(source) - end) and (
                    len(end_errors) == 0 or
                    length + len(source[current])) < bound:
                end_errors.append(source[current])
                length += len(source[current])
                current -= 1
            result.extend(reversed(end_errors))

        return length, result


    remaining_length = max_error_len
    if install_stdout or install_errors:
        result.append(INSTALL_ERROR_START)
        used, lines = bound_errors(install_stdout, 0.2 * max_error_len)
        remaining_length -= used
        result.extend(lines)
        result.append(INSTALL_ERROR_MID)
        used, lines = bound_errors(install_errors,
                                   max(0.3 * max_error_len,
                                       0.5 * max_error_len - used))
        remaining_length -= used
        result.extend(lines)
        result.append(INSTALL_ERROR_END)

    _, lines = bound_errors(language_detection_errors + compile_errors, remaining_length)
    result.extend(lines)
    return result


def compile_anything(bot_dir, installTimeLimit=600, timelimit=600, max_error_len=10*1024):
    install_stdout = []
    install_errors = []
    if os.path.exists(os.path.join(bot_dir, "install.sh")):
        install_stdout, install_errors = _run_cmd(
            "chmod +x install.sh; ./install.sh", bot_dir, installTimeLimit)

    detected_language, language_errors = detect_language(bot_dir)

    print("detected language " + str(detected_language))
    if not detected_language:
        return "Unknown", truncate_errors(install_stdout, install_errors,
                                          language_errors, [], max_error_len)

    print("compiling")
    compiled, compile_errors = compile_function(detected_language, bot_dir, timelimit)
    if not compiled:
        return (detected_language.name,
                truncate_errors(install_stdout, install_errors,
                                language_errors, compile_errors, max_error_len))

    print("done compiling")
    name = detected_language.name
    run_cmd = detected_language.command
    run_filename = os.path.join(bot_dir, 'run.sh')
    print("filename:")
    print(run_filename)
    try:
        with open(run_filename, 'wb') as f:
            f.write(('#%s\n%s\n' % (name, run_cmd)).encode('utf-8'))
        print("file:")
        with open(run_filename, 'r') as f:
            for line in f:
                print(line)
    except Exception as e:
        print("error")
        print(e)

    # allow LANGUAGE file to override language name
    override_name = detect_language_file(bot_dir)
    if override_name:
        name = override_name
    return name, None

if __name__ == "__main__":
    if len(sys.argv) > 1:
        workingPath = sys.argv[1]
        lang, errors = compile_anything(workingPath)
        print("Language:", lang)
        if errors:
            print("--- Errors: ---")
            print("\n".join(errors))
