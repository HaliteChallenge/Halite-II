# Adding a New Language to Halite

1. Update `setup_worker_image.sh` to install the new runtime/compiler. You should add the package (if it's a repository package) to the `PACKAGES` variable, as it will print out the versions of everything installed in the end for documentation.
2. Follow the instructions in `INSTALL.md` to create a new worker image.
3. Delete the worker instance group and instance template in Google Cloud.
4. Update `setup_workers.sh` to use the new worker image.
5. Recreate the worker instance template and group.