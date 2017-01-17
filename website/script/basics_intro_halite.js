$(function() {
    textFromURL("ar1481484242-3993659735.hlt", $("#gameReplay"), function(data) {
        console.log(data)
        if(data != null) {
            showGame(data, $("#gameReplay"), null, 500, true, true);
        }
    });
})
