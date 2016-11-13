$(function () {
    var $dropZone = $("#dropZone");
    var $filePicker = $("#filePicker");
    function handleFiles(files) {
        for(var i=0, file; file=files[i]; i++) {
            console.log(file)
            var reader = new FileReader();

            reader.onload = (function(filename) { // finished reading file data.
                return function(e2) {
                    $dropZone.remove();
                    showGame(textToGame(e2.target.result, filename), $("#pageContent"), null, null, true, false, true);
                };
            })(file.name);
            reader.readAsText(file); // start reading the file data.
        }
    }
    $("#pageContent").append($dropZone);

    $dropZone.on('dragover', function(e) {
        e.stopPropagation();
        e.preventDefault();
    });
    $dropZone.on('drop', function(e) {
        e.stopPropagation();
        e.preventDefault();
        var files = e.originalEvent.dataTransfer.files; // Array of all files
        handleFiles(files)
    });
    $filePicker.on('change', function(e) {
        var files = e.target.files
        handleFiles(files)
    });
})
