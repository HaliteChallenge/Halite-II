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
INCLUDED_EXTENSIONS = [".py", ".java", ".cpp", ".hpp", ".cs", ".csproj"]
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

    # TODO: also get sample bots and READMEs

    return included_files


def make_archive(output, environment, base_path, included_files):
    """Create the output ZIP archive."""
    platform, source, target = environment
    with zipfile.ZipFile(output, "w", zipfile.ZIP_DEFLATED) as archive:
        if source is not None:
            # source is None <=> platform-agnostic archive
            archive.write(source, target)

        for file in included_files:
            target_path = os.path.relpath(file, base_path)
            archive.write(file, target_path)

        if "Windows" in platform:
            run_game = os.path.join(base_path, "run_game.bat")
            if os.path.isfile(run_game):
                archive.write(run_game, "run_game.bat")
        else:
            run_game = os.path.join(base_path, "run_game.sh")
            if os.path.isfile(run_game):
                archive.write(run_game, "run_game.sh")


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

    make_source_download()

    # Keep paths of all source files around so we can make a single combined
    # download at the end
    all_files = []

    for directory in os.listdir(STARTER_KIT_DIR):
        full_path = os.path.join(STARTER_KIT_DIR, directory)
        if not os.path.isdir(full_path):
            continue

        if directory == "sample_bots":
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
        })

    output["languages"].append({
        "language": "(all languages)",
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
