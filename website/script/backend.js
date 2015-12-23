var url = "/Halite/website/php/"
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
}

function storeUserSession(username, password, async) {
	$.ajax({
		url: url+"session", 
		async: async,
		method: "POST",
		data: {username: username, password: password}
    });
}

function storeBot(userID, name, async) {
	$.ajax({
		url: url+"bot", 
		async: async,
		method: "POST",
		data: {name: name, userID: userID}
    });
}

// Uploads source code for users's bot
// When given the html ID of a form with the userID as a value
// and the forms as another value
function storeBotFiles(formID) {
	if(!$("#"+formID).find("input[name='userID']").length || $("#"+formID).find("input[name='userID']").val().localeCompare("") == 0) {
		console.log("Form not setup correctly. Does not include userID");
		throw 1;
	}
	var formData = new FormData($("#"+formID)[0]);
	console.log(formData);
	var result = $.ajax({
        url: url+"botFiles",
        type: "POST",
        async: false,
        data: formData,
        processData: false,
        contentType: false,
        xhr: function() {
            var myXhr = $.ajaxSettings.xhr();
            return myXhr;
        },
        success: function (data) {
            alert("Data Uploaded: "+data);
        },
		error: function (xhr, ajaxOptions, thrownError) {
			console.log(xhr.responseText);
			console.log(thrownError);
		}
    });
    console.log(result)
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

function getBot(botID, name) {
	var data = null;
	if(typeof botID !== "undefined") data = {boID: botID};
	else data = {name: name};

	var result = $.ajax({
		url: url+"bot", 
		async: false,
		data: data
    });

	return result.responseJSON;
}

function getUserBots(userID) {
	var result = $.ajax({
		url: url+"bot", 
		async: false,
		data: {userID: userID}
    });

	return result.responseJSON;
}

function destroySession(async) {
	$.ajax({
		url: url+"session", 
		async: async,
		method: "DELETE"
    });
}
