import unittest
import sys
import os
import glob
import zipfile
import shutil

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', 'worker'))
import compiler

class CompilerTests(unittest.TestCase):
    def testStarterPackages(self):
        # Archive the starter packages like the website would
        SP_DIR = "starterpackages"
        if os.path.isdir(SP_DIR):
            shutil.rmtree(SP_DIR)
        os.system("cd ../../website/; ./archiveStarterPackages.sh");
        shutil.copytree("../../website/downloads/starterpackages/", SP_DIR)
        for f in glob.glob(os.path.join(SP_DIR, "*.zip")):
            zip_ref = zipfile.ZipFile(f, 'r')
            zip_ref.extractall(SP_DIR)
            zip_ref.close()

            folderName = os.path.splitext(os.path.basename(f))[0]
            folder = os.path.join(SP_DIR, os.path.splitext(os.path.basename(f))[0])
            expectedLanguage = folderName.split("-")[1]
            language, errors = compiler.compile_anything(folder)
            print(errors)
            print(language)

            assert language == expectedLanguage
            assert errors == None

if __name__ == '__main__':
    unittest.main()
