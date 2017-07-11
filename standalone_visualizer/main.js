const path = require("path");
const url = require("url");

const electron = require("electron");

// Keep global reference to Electron window, because it'll be closed when
// garbage collected
let mainWindow;

function createWindow() {
    mainWindow = new electron.BrowserWindow({
        width: 1000,
        height: 960,
    });

    mainWindow.loadURL(url.format({
        pathname: path.join(__dirname, "index.html"),
        protocol: "file:",
        slashes: true,
    }));

    mainWindow.on("closed", function() {
        // Close the window by letting JS GC it
        mainWindow = null;
    });
}

electron.app.on("ready", createWindow);
electron.app.on("window-all-closed", function() {
    if (process.platform !== "darwin") {
        app.quit();
    }
});
electron.app.on("activate", function() {
    if (mainWindow === null) {
        createWindow();
    }
});
