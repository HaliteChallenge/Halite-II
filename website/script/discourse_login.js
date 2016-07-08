(function() {
  var logInForm = {
    $logInUsername: $("#login_user"),
		$logInPassword: $("#login_pass"),
		$logInButton: $("#loginButton"),
    user: null,
    init: function(onLogin) {
      this.onLogin = onLogin;
      new SmartForm(this.$logInButton, this.$logInForm, this.logIn.bind(this));
    },
    logIn: function() {
			messageBox.clear();

			var user = getUser(null, this.$logInUsername.val(), this.$logInPassword.val());
			if(user == null) {
				messageBox.alert("Login failed", "That username/password combo does not exist", false);
			} else if(user['isVerified'] == false) {
				messageBox.alert("Login failed", "Your email needs to be verified", false);
			} else {
				storeUserSession(this.$logInUsername.val(), this.$logInPassword.val(), false);
				this.loggedIn = true;
				this.user = user;

        this.onLogin();
			}
		},
    logIntoForum: function() {

    }
  };

  var payload = getGET("sso");
  var signature = getGET("sig");
  var session = getSession();
  if(session == null) {
    logInForm.init(function() {
      window.location.href = getForumSignInURL(payload, signature, logInForm.user.userID, logInForm.user.email, logInForm.user.username);
    });
  } else {
    window.location.href = getForumSignInURL(payload, signature, logInForm.user.userID, logInForm.user.email, logInForm.user.username);
  }
})
