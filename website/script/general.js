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
	return results === null ? null : decodeURIComponent(results[1].replace(/\+/g, " "));
}

// The message box should be accessible to all pages
var messageBox = {
	$messageBox: $("#messageBox"),
	alert: function(title, message, isSuccess) {
		this.clear()
		this.$messageBox.append($("<div class='alert "+(isSuccess ? "alert-success" : "alert-danger")+" alert-dismissible' role='alert'><button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button><strong>"+title+"</strong>&nbsp;&nbsp;"+message+"</div>"))
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
		$logInUsername: $("#login_user"),
		$logInPassword: $("#login_pass"),
		$logInButton: $("#login_button"),
		$logInForm: $("#login_form"),
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
			init: function() {
				this.$button.click(this, this.buttonClicked.bind(this));
				this.$fileInput.change(this, this.fileChanged.bind(this));
			},
			setCredentials: function(userID, password) {
				this.$form.append("<input type='hidden' name='userID' value='"+userID+"'>");
				this.$form.append("<input type='hidden' name='password' value='"+password+"'>");
			},
			buttonClicked: function() { this.$fileInput.click(); },
			fileChanged: function() {
				storeBotFile("submitForm");
				messageBox.alert("Bot Submitted", "Your bot was successfully uploaded to our servers. <b>If your bot does not compile, you will receive an email in a couple of minutes.</b> Otherwise, you will show up on the leaderboard very soon.", true)
			}
		},

		init: function() {
			new SmartForm(this.$logInButton, this.$logInForm, this.logIn.bind(this));
			new SmartForm(this.$registerButton, this.$registerForm, this.register.bind(this));

			this.uploadButton.init();
			this.$logOutButton.click(this.logOut.bind(this));

			var session = getSession();
			if(session != null && session.userID != null) {
				this.user = session;
				this.loggedIn = true;
			}

			this.render();
		},
		logIn: function() {
			messageBox.clear();

			var user = getUser(null, this.$logInUsername.val(), this.$logInPassword.val());
			if(user == null) {
				messageBox.alert("Login failed", "That username/password combo does not exist", false);
			} else if(user['isVerified'] == false) {
				messageBox.alert("Login failed", "Your email needs to be verified", false);
			} else {
				messageBox.alert("Login successful", "You are now logged in as <b>"+user.username+"</b>.", true);
				storeUserSession(this.$logInUsername.val(), this.$logInPassword.val(), false);
				this.loggedIn = true;
				this.user = user;
				this.render();
			}
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

	var doLogOff = getGET("forumsLogOut");
	if(doLogOff != null && doLogOff != undefined) {
		messageBox.alert("Logged Out", "You have been logged out of forums.halite.io and halite.io", true);
		destroySession(false);
	}

	navbar.init();
})
