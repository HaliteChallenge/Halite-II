// Google analytics
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-80656237-1', 'auto');
ga('send', 'pageview');

function getGET(name) {
    name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
    results = regex.exec(location.search);
    return results === null ? null : decodeURIComponent(results[1]);
}

// The message box should be accessible to all pages
var messageBox = {
    $messageBox: $("#messageBox"),
    alert: function(title, message, isSuccess, onClose) {
        this.clear()
        this.$messageBox.append($("<div class='alert "+(isSuccess ? "alert-success" : "alert-danger")+" alert-dismissible' role='alert'><button type='button' class='close' id='messageCloseButton' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button><strong>"+title+"</strong>&nbsp;&nbsp;"+message+"</div>"))
        if(onClose != null) $("#messageCloseButton").click(onClose);
    },
    clear: function() {
        this.$messageBox.empty()
    }
};

function SmartForm($submitButton, $form, onSubmit) {
    $submitButton.click(function() {
        console.log("CLICK");
        onSubmit();
    });
    $form.keypress(function(event) {
        if (event.which == 13) {
            event.preventDefault();
            onSubmit();
        }
    });
};

$(function() {
    var navbar = {
        loggedIn: false,
        $registerUsername: $("#register_user"),
        $registerEmail: $("#register_email"),
        $registerPassword: $("#register_pass"),
        $registerConfirmPassword: $("#register_confirm_pass"),
        $registerButton: $("#register_button"),
        $registerForm: $("#register_form"),
        $logInNav: $("#loginNav"),
        $logOutNav: $("#logoutNav"),
        $logOutButton: $("#logoutButton"),

        uploadButton: {
            $button: $("#submitButton"),
            $form: $("#submitForm"),
            $fileInput: $("#myFile"),
            $submitModal: $("#submitModal"),
            $submitModalButton: $("#submitModalButton"),
            init: function(session) {
                this.session = session;
                this.$button.click(this, this.buttonClicked.bind(this));
                this.$submitModalButton.click(this, this.submitModal.bind(this));
                this.$fileInput.change(this, this.fileChanged.bind(this));
            },
            setCredentials: function(userID, password) {
                this.$form.append("<input type='hidden' name='userID' value='"+userID+"'>");
                this.$form.append("<input type='hidden' name='password' value='"+password+"'>");
            },
            buttonClicked: function() { 
                var user = getUser(this.session.userID);
                if(parseInt(user.isRunning) == 1) {
                    this.$fileInput.click(); 
                } else {
                    this.$submitModal.modal("show");
                }
            },
            submitModal: function() { 
                this.$fileInput.click(); 
            },
            fileChanged: function() {
                this.$submitModal.modal("hide");
                try {
                    var uploadOutput = storeBotFile("submitForm");
                    if(uploadOutput.indexOf("desktop") != -1) {
                        messageBox.alert("Restricted Access", "You are not allowed to submit code to Halite from a Two Sigma Desktop. Please use a personal computer on Two Sigma Wifi instead.", false);
                    } else if(uploadOutput.indexOf("large") != -1) {
                        messageBox.alert("File Size Error", "Your bot file was too big. We only allow submissions less than 20 megabytes. Make sure you aren't packaging unnecessary binaries.", false);
                    } else if(uploadOutput.indexOf("Compiling") != -1) {
                        messageBox.alert("Compiling", "We are compiling one of your bots. You have to wait until we have finished compiling your bot before you may submit another one.", false);
                    } else {
                        messageBox.alert("Bot Submitted", "Your bot was successfully uploaded to our servers. <b>If your bot does not compile, you will receive an email in a couple of minutes.</b> Otherwise, you will show up on the leaderboard very soon.", true)
                    }
                } catch (err) {
                    messageBox.alert("File Upload Error", "An error occurred while uploading your file. <b>Your file may have been too big</b>. Check to make sure that your file is under <b>20 megabytes</b>. Make sure that you haven't packaged some unnecessary, big binaries. If this persists, post of the forums or email us at halite@halite.io.", false);
                }
            }
        },

        init: function(session) {
            new SmartForm(this.$registerButton, this.$registerForm, this.register.bind(this));

            this.uploadButton.init(session);
            this.$logOutButton.click(this.logOut.bind(this));

            if(session != null && session.userID != null) {
                this.user = session;
                this.loggedIn = true;
            }

            this.render();
        },
        register: function() {
            messageBox.clear();

            var username = this.$registerUsername.val();
            var email = this.$registerEmail.val();
            var password = this.$registerPassword.val();
            var confirmPassword = this.$registerConfirmPassword.val();

            if(password != confirmPassword) {
                messageBox.alert("Passwords Don't Match", "Please type in your password correctly.", false);
            } else {
                var resp = storeUserDatabase(email, username, password, false);
                console.log(resp)
                if (resp === "Success") {
                    messageBox.alert("Verify Your Email", "You may not log in until you verify your email.", true);
                    storeUserSession(username, password, false);
                } else  {
                    if(resp.toLowerCase().indexOf("username") > -1) {
                        messageBox.alert("Registration failed", "That username is already taken", false);
                    } else if (resp.toLowerCase().indexOf("exists") > -1) {
                        messageBox.alert("Registration failed", "That email is already taken", false);
                    } else if (resp.toLowerCase().indexOf("sigma") > -1) {
                        messageBox.alert("Registration failed", "That email does not end in twosigma.com", false);
                    } else {
                        messageBox.alert("Registration failed", "That email is invalid", false);
                    }
                }
            }
        },
        logOut: function() {
            destroySession(false);
            this.loggedIn = false;
            this.render();
        },
        render: function() {
            if(this.loggedIn) {
                this.$logInNav.css("display", "none");
                this.$logOutNav.css("display", "inline");

                this.uploadButton.setCredentials(this.user.userID, this.user.password);
            } else {
                this.$logInNav.css("display", "inline");
                this.$logOutNav.css("display", "none");
            }
        }
    }

    if(getGET("forumsLogOut") != null) {
        messageBox.alert("Logged Out", "You have been logged out of forums.halite.io and halite.io", true);
        destroySession(false);
    }
    if(getGET("unsubscribeEmails")) {
        messageBox.alert("Unsubscribed", "You have unsubscribed from all halite emails.", true)
    }
    if(getGET("subscribeEmails")) {
        messageBox.alert("Subscribed", "You have subscribed to all halite emails!", true)
    }

    var session = getSession();
    navbar.init(session);
    if(session != null) {
        var announcement = getLatestAnnouncement(session.userID);
        console.log(announcement)
        if(announcement != null) {
            messageBox.alert(announcement.header, announcement.body, true, function() {
                closedAnnouncement(announcement.announcementID);  
            });
        }
    }

})
