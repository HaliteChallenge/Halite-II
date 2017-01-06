$(function () {
    var $dropZone = $("html");
    var $filePicker = $("#filePicker");
    function handleFiles(files) {
        // only use the first file.
        file = files[0];
        console.log(file)
        var reader = new FileReader();

        reader.onload = (function(filename) { // finished reading file data.
            return function(e2) {
                $("#displayArea").empty();
                $("label[for=filePicker]").text("Select another file");
                var fsHeight = $("#fileSelect").outerHeight();
                showGame(textToGame(e2.target.result, filename), $("#displayArea"), null, -fsHeight, true, false, true);
            };
        })(file.name);
        reader.readAsText(file); // start reading the file data.
    }

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
