$(function() {
    var data = textFromURL("ar1482413174-917235674.hlt", $("#gameReplay"), function(data) {
        console.log(data)
        if(data != null) {
            showGame(data, $("#gameReplay"), null, 500, true, true);
        }
    });
})
