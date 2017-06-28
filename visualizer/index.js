function showGame(buffer, displayArea) {
    let replay;
    try {
        const inflated = pako.inflate(buffer);
        console.log("Compressed replay");
        replay = msgpack.decode(inflated);
    }
    catch (e) {
        console.log("Uncompressed replay");
        replay = msgpack.decode(buffer);
    }

    console.log(replay);

    const visualizer = new HaliteVisualizerControls(replay);
    visualizer.attach(displayArea);
    return visualizer;
}

$(function () {
    const fs = require('fs');
    const args = require('electron').remote.process.argv;

    const displayArea = $("#displayArea");
    let visualizer;

    if (args.length > 2) {
        // TODO: make this work
        fs.readFile(args[2], 'utf-8', function (err, data) {
            if(err){
                alert("An error ocurred reading the file :" + err.message);
                return;
            }
            $("label[for=filePicker]").text("Select another file");
            let fsHeight = $("#fileSelect").outerHeight();

            visualizer = showGame(data, displayArea);
        });
    }

    const $dropZone = $("html");
    const $filePicker = $("#filePicker");
    function handleFiles(files) {
        // only use the first file.
        const file = files[0];
        console.log(file);
        const reader = new FileReader();

        reader.onload = (function(filename) { // finished reading file data.
            return function(e2) {
                displayArea.empty();
                $("label[for=filePicker]").text("Select another file");
                let fsHeight = $("#fileSelect").outerHeight();

                visualizer = showGame(e2.target.result, displayArea);
            };
        })(file.name);
        reader.readAsArrayBuffer(file); // start reading the file data.
    }

    $dropZone.on('dragover', function(e) {
        e.stopPropagation();
        e.preventDefault();
    });
    $dropZone.on('drop', function(e) {
        e.stopPropagation();
        e.preventDefault();
        const files = e.originalEvent.dataTransfer.files; // Array of all files
        handleFiles(files);
    });
    $filePicker.on('change', function(e) {
        const files = e.target.files;
        handleFiles(files);
    });
});
