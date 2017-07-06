<template>
    <table class="table table-striped table-hover">
    </table>
</template>

<script>
    import * as libhaliteviz from "libhaliteviz";
    console.log(libhaliteviz);

    function showGame(buffer, displayArea) {
        let replay = libhaliteviz.parseReplay(buffer);

        console.log(replay);

        const visualizer = new libhaliteviz.HaliteVisualizerControls(replay);
        visualizer.attach(displayArea);
        return visualizer;
    }

    const $dropZone = $("html");
    const $filePicker = $("#filePicker");
    const displayArea = $("#container");

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

    export default {
        name: "visualizer",
        data: function() {
            return {
                leaderboard: [],
            };
        },
        mounted: function() {
        },
    }
</script>

<style lang="scss" scoped>

</style>