### Offline Visualiser

#### Building the electron app

1. Install node packages for the website
    ```
    cd ../../website
    npm install
    ```
    
2. Install node packages for libhaliteviz
    ```
    cd ../libhaliteviz
    npm install
    ```
    
3. Install node packages for libhaliteviz, and run build
    ```
    cd ../tools/standalone_visualizer
    npm install
    npm run build
    ```
4. Start the app
    ```
    npm start
    ```

#### Packaging

We use `electron-packager` to package and distribute the electron app. The command is the following basic form:
      
```
electron-packager <sourcedir> <appname> --platform=<platform> --arch=<arch> [optional flags...]
```
