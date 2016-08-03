$(function() {
	var registerForm = {
		$registerUsername: $("#register_user"),
		$registerEmail: $("#register_email"),
		$registerPassword: $("#register_pass"),
		$registerConfirmPassword: $("#register_confirm_pass"),
		$registerButton: $("#register_button"),
		$registerForm: $("#register_form"),
		user: null,
		init: function(onRegister) {
			this.onRegister = onRegister;
			new SmartForm(this.$registerButton, this.$registerForm, this.register.bind(this));
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
		}
	};

	registerForm.init(function() {
		handleForumsSignIn(payload, signature, registerForm.user);
	});
})
