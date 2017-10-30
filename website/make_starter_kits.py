#!/usr/bin/env python3
"""
Generate starter kit downloads and a download page.
"""

import argparse
import glob
import json
import os
import zipfile


ENVIRONMENT_DIR_HELP = "Directory containing precompiled Halite environment " \
                       "executables, each named after their platform. "
VERSION_HELP = "The version string to embed in the downloads page."
IGNORED_EXTENSIONS = [".exe", ".class", ".pyc", ".obj"]
INCLUDED_EXTENSIONS = [".py", ".java", ".cpp", ".hpp", ".cs", ".csproj", ".scala", ".js", ".sh", ".bat", ".toml", ".rs",".go",".txt",".rb", ".kt"]
INCLUDED_FILES = ["Makefile", "README"]
STARTER_KIT_DIR = "../airesources"
DOWNLOAD_DATA = "_data/downloads.json"
PLATFORM_AGNOSTIC = "None"

# Names of generated downloads
# Standard language + platform
OUTPUT_FILE_FORMAT = "assets/downloads/Halite2_{language}_{platform}.zip"

# Platform only
ENVIRONMENT_OUTPUT_FILE_FORMAT = "assets/downloads/Halite2_{platform}.zip"

# All languages + platform
ALL_LANGUAGES_OUTPUT_FILE_FORMAT = "assets/downloads/Halite2_all_{platform}.zip"
SOURCE_FILE = "assets/downloads/Halite2Source.zip"

versions =  {"Python3" : "1.0", "C++" : "1.0", "Java" : "1.0", "CSharp" : "0.9.0-beta" ,"JavaScript": "0.9-beta",
"ML-StarterBot-Python":"1.0", "Rust" : "0.9.0-beta", "Scala" : "0.9.0-beta", "Go" : "0.9.0-beta", "Ruby" : "0.9.0-beta" , "Kotlin" : "0.9.0-beta" }


def detect_environments(directory):
    """Detect which platform binaries we have."""
    environments = [(PLATFORM_AGNOSTIC, None, None)]
    for filename in os.listdir(directory):
        platform, platform_ext = os.path.splitext(filename)

        if platform == ".DS_Store":
            # Dang it, MacOS
            continue

        print("Detected platform", platform)
        environments.append((platform,
                             os.path.join(directory, filename),
                             "halite" + platform_ext))

    return environments


def scan_directory(full_path):
    """Figure out what the starter kit files in a directory are."""

    included_files = []
    for containing_dir, _, possible_files in os.walk(full_path):
        for filename in possible_files:
            _, ext = os.path.splitext(filename)
            if ext.lower() in INCLUDED_EXTENSIONS or filename in INCLUDED_FILES:
                included_files.append(os.path.join(containing_dir, filename))

    included_files.append(os.path.join(STARTER_KIT_DIR, "README.MD"))
    return included_files


def make_archive(output, environment, base_path, included_files):
    """Create the output ZIP archive."""
    platform, source, target = environment
    # Get rid of duplicates
    included_files = list(set(included_files))
    with zipfile.ZipFile(output, "w", zipfile.ZIP_DEFLATED) as archive:
        if source is not None:
            # source is None <=> platform-agnostic archive
            zinfo = zipfile.ZipInfo.from_file(source, target)
            zinfo.compress_type = zipfile.ZIP_DEFLATED
            zinfo.external_attr = 0o0100755 << 16
            with open(source, 'rb') as source_file:
                archive.writestr(zinfo, source_file.read())

        for file in included_files:
            target_path = os.path.relpath(file, base_path)
            archive.write(file, target_path)


def make_source_download():
    included_files = []

    for directory, _, file_list in os.walk("../environment"):
        target_dir = os.path.relpath(directory, "../environment")
        for filename in file_list:
            _, ext = os.path.splitext(filename)
            if ext.lower() in {".cpp", ".c", ".hpp", ".h", ".bat"} or \
                    filename == "Makefile":
                source_path = os.path.join(directory, filename)
                target_path = os.path.normpath(
                    os.path.join("Halite/", target_dir, filename))

                included_files.append((source_path, target_path))

    with zipfile.ZipFile(SOURCE_FILE, "w", zipfile.ZIP_DEFLATED) as archive:
        for source_path, target_path in included_files:
            archive.write(source_path, target_path)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("version", help=VERSION_HELP)
    parser.add_argument("environment_dir", help=ENVIRONMENT_DIR_HELP)

    args = parser.parse_args()

    environments = detect_environments(args.environment_dir)
    generated_languages = []

    # Ensure output directories exists
    for location in [SOURCE_FILE, DOWNLOAD_DATA]:
        try:
            os.makedirs(os.path.dirname(location))
        except FileExistsError:
            pass

    make_source_download()

    # Keep paths of all source files around so we can make a single combined
    # download at the end
    all_files = []

    for directory in os.listdir(STARTER_KIT_DIR):
        full_path = os.path.join(STARTER_KIT_DIR, directory)
        if not os.path.isdir(full_path):
            continue

        if directory == "starterkitdocs":
            continue

        language = directory
        generated_languages.append(language)
        print("Language:", language)

        included_files = scan_directory(full_path)
        for file in included_files:
            print("\tIncluding:", file)

        print()

        all_files.extend(included_files)

        for (platform, source, target) in environments:
            output = "./" + OUTPUT_FILE_FORMAT.format(
                language=language, platform=platform)
            print("\tMaking:", output)
            make_archive(output, (platform, source, target),
                         full_path, included_files)

    panlanguage_kits = []
    for (platform, source, target) in environments:
        # Make downloads including all languages
        filename = ALL_LANGUAGES_OUTPUT_FILE_FORMAT.format(platform=platform)
        all_output = "./" + filename
        print("\tMaking:", all_output)
        make_archive(all_output, (platform, source, target), "../airesources", all_files)
        panlanguage_kits.append(filename)

        # Make downloads including no languages
        if source is None:
            continue
        output = "./" + ENVIRONMENT_OUTPUT_FILE_FORMAT.format(platform=platform)
        print("\tMaking:", output)
        make_archive(output, (platform, source, target), "", [])

    output = {
        "platforms": [environment[0] for environment in environments],
        "languages": [],
        "environments": [],
        "source": SOURCE_FILE,
        "version": args.version,
    }
    generated_languages.sort()
    for language in generated_languages:
        language_kits = []
        for (platform, _, _) in environments:
            language_kits.append(
                OUTPUT_FILE_FORMAT.format(
                    language=language, platform=platform))

        output["languages"].append({
            "language": language,
            "files": language_kits,
            "version": versions[language]
        })

    output["languages"].append({
        "language": "All Languages",
        "files": panlanguage_kits,
    })

    for (platform, source, _) in environments:
        if source is None:
            continue
        output["environments"].append(
            ENVIRONMENT_OUTPUT_FILE_FORMAT.format(platform=platform))

    with open(DOWNLOAD_DATA, "w") as output_file:
        json.dump(output, output_file, indent=2)


if __name__ == "__main__":
    main()
