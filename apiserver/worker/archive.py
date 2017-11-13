import zipfile
import os
import platform
import shutil


def unpack(filePath):
    """Unpacks and deletes a zip file into the files current path"""
    folderPath = os.path.dirname(filePath)

    with zipfile.ZipFile(filePath) as f:
        f.extractall(folderPath)

    # Remove __MACOSX folder if present
    macFolderPath = os.path.join(folderPath, "__MACOSX")
    if os.path.exists(macFolderPath) and os.path.isdir(macFolderPath):
        shutil.rmtree(macFolderPath)

    os.remove(filePath)


def zipFolder(folderPath, destinationFilePath):
    """Zips a folder to a path"""
    zipFile = zipfile.ZipFile(destinationFilePath, "w", zipfile.ZIP_DEFLATED)

    originalDir = os.getcwd()
    try:
        os.chdir(folderPath)

        for rootPath, dirs, files in os.walk("."):
            for file in files:
                if os.path.basename(file) != os.path.basename(destinationFilePath):
                    zipFile.write(os.path.join(rootPath, file))

        zipFile.close()
    finally:
        os.chdir(originalDir)
