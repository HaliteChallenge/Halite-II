$(function() {
    leaderTable.init(getActiveUsers());

    if(getGET("userID") != null && getGET("verificationCode") != null) {
        var res = verifyUser(parseInt(getGET("userID")), getGET("verificationCode"));
        if(res == "Success") {
            messageBox.alert("Email Verification Successful", "You may now log into your Halite account and submit to the competition!", true);
        } else {
            messageBox.alert("Email Verification Error", "An error occured while trying to verfy your email. If this problem is persistent, please email halite@halite.io", false);
        }
    }
})
