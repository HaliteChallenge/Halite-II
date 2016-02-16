var url = "php/website/";
// Attempts to store the username/password combo given
// Returns false if the username is already taken
// If async returns null
function storeUserDatabase(username, password, async) {
	var result = $.ajax({
		url: url+"user", 
		async: async,
		method: "POST",
		data: {username: username, password: password}
	});

	if(async == true) {
		return null;
	}
	return result.responseJSON;
}

function storeUserSession(username, password, async) {
	$.ajax({
		url: url+"session", 
		async: async,
		method: "POST",
		data: {username: username, password: password}
	});
}

// Uploads source code for users's bot
// When given the html ID of a form with the userID as a value and the file as another value
function storeBotFile(formID) {
	if(!$("#"+formID).find("input[name='userID']").length || $("#"+formID).find("input[name='userID']").val().localeCompare("") == 0) {
		console.log("Form not setup correctly. Does not include userID");
		throw 1;
	}
	var formData = new FormData($("#"+formID)[0]);
	var result = $.ajax({
		url: url+"botFile", 
		async: false,
		method: "POST",
		data: formData,
		processData: false,
		contentType: false,
		xhr: function() {
			var myXhr = $.ajaxSettings.xhr();
			return myXhr;
		},
		success: function(result) {
			console.log(result);
		},
		error: function (xhr, ajaxOptions, thrownError) {
			console.log(xhr.responseText)
		}
	})
	return result.responseJSON;
}

function getSession() {
	var result =  $.ajax({ 
		url: url+"session", 
		async: false,
		method: 'GET'
	});
	return result.responseJSON;
}

function getUser(userID, username, password) {
	var result = $.ajax({
		url: url+"user", 
		async: false,
		method: "GET",
		data: {userID: userID, username: username, password: password}
	});

	return result.responseJSON;
}

function getActiveUsers() {
	var result = $.ajax({
		url: url+"user", 
		async: false,
		method: "GET",
		data: {active: 1}
	});
	console.log(result)
	console.log(result.responseJSON)
	return result.responseJSON;
}

function getLatestGamesForUser(userID) {
	var result = $.ajax({
		url: url+"game", 
		async: false,
		method: "GET",
		data: {userID: userID}
	});
	return result.responseJSON;
}

function getGameFile(filename) {
	var result = $.ajax({
		url: "../storage/replays/"+filename, 
		async: false,
		method: "GET"
	});
	console.log(result.responseText)
	return result.responseText;
}

function destroySession(async) {
	$.ajax({
		url: url+"session", 
		async: async,
		method: "DELETE"
	});
}
