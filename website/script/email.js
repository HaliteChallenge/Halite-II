$(function() {
    var githubEmailForm = {
        $submitButton: $("#githubSubmitButton"),
        $emailLoc: $("#emailLoc"),
        init: function(email, submitCallback) {
            this.email = email;
            this.submitCallback = submitCallback;
            this.$submitButton.click(this, this.submitCallback.bind(this));         

            this.render();
        },
        render: function() {
            this.$emailLoc.html(this.email);
        }
    }

    var customEmailForm = {
        $firstField: $("#firstField"),
        $secondField: $("#secondField"),
        $messageBox: $("#messageBox"),
        $submitButton: $("#customSubmitButton"),
        init: function(submitCallback) {
            this.submitCallback = submitCallback;
            this.$submitButton.click(this, this.onClick.bind(this));         
        },
        render: function() {
            this.$emailLoc.html(this.email);
        },
        onClick: function() {
            if(this.$firstField.val() != this.secondField.val()) {
                this.displayMessage("Email Mismatch", "The two emails that you entered do not match.", false);
            } else if(this.$firstField.val() == "" || this.secondField.val() == "") {
                this.displayMessage("Empty Fields", "Please fill your email twice in the boxes below.", false);
            } else {
                this.submitCallback(this.$firstField.val());
            }
        },
        displayMessage: function(title, message, isSuccess) {
            this.$messageBox.append($("<div class='alert "+(isSuccess ? "alert-success" : "alert-danger")+" alert-dismissible' role='alert'><button type='button' class='close' id='messageCloseButton' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button><strong>"+title+"</strong>&nbsp;&nbsp;"+message+"</div>"))
        }
    }

    var user = getUser(getSession()['userID']);
    console.log(user);
    //if(user == null) window.location.href = "index.php";
    if(parseInt(user.isEmailGood) == 0 && user.email != null && user.email != undefined) {
        $("#forms").css("display", "none");
        $("#waitMessage").css("display", "block");
    }

    githubEmailForm.init(user.email, function() {
        validateEmail();
        //window.location.href = "index.php";
    });
    customEmailForm.init(function(email) {
        newEmail(email);
        location.reload();
    });
});
