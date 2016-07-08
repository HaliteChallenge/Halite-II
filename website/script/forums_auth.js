$(function() {
  var logInForm = {
    $logInForm: $("#login_form"),
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
		}
  };

  function handleForumsSignIn(payload, signature, user) {
    var url = getForumSignInURL(payload, signature, user.userID, user.email, user.username);
    console.log(url);
    if(url == null || url == undefined) {
      messageBox.alert("Forums Login Failed", "An error occured while trying to log you into forums.halite.io", false);
    } else {
      window.location.href = url;
    }
  }

  var payload = getGET("sso");
  var signature = getGET("sig");
  console.log(payload);
  console.log(signature);

  var session = getSession();
  logInForm.init(function() {
    handleForumsSignIn(payload, signature, logInForm.user);
  });
  if(session != null) {
    handleForumsSignIn(payload, signature, session);
  }
})
