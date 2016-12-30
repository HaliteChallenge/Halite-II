$(function() {
    $("#numUsers").html(getNumActiveUsers());
    var data = textFromURL("ar1482947270-2437412300.hlt", $("#gameReplay"), function(data) {
        console.log(data)
        if(data != null) {
            showGame(data, $("#gameReplay"), null, 500, true, true, true, 30);
        }
    });
})
