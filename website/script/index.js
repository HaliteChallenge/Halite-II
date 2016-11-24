$(function() {
    var data = textFromURL("ar1478846062-2923329127.hlt", $("#gameReplay"), function(data) {
        console.log(data)
        if(data != null) {
            showGame(data, $("#gameReplay"), null, 500, true, true);
        }
    });
})
