// Attempts to store the username/password combo given
// Returns false if the username is already taken
function putUsernamePasswordDatabase(username, password) {
	var result = $.ajax({
		url: "Backend/user", 
		async: false,
		method: "POST",
		data: {username: username, password: password}
    });

    if(result.responseText.localeCompare("ERROR") || result.responseText.localeCompare("null")) {
		return false;
	}
	return true;
}

function putUsernamePasswordSession(username, password, async) {
	$.ajax({
		url: "Backend/session", 
		async: async,
		method: "POST",
		data: {username: username, password: password}
    });
}

function getSession() {
	var response =  $.ajax({ url: "Backend/session", async: false });
	if(response.responseText.localeCompare("null") == 0 || Object.keys(response.responseJSON).length === 0) {
		return null;
	}
	return response.responseJSON;
}

function getUserCredentials(username, password) {
	var result = $.ajax({
		url: "Backend/user", 
		async: false,
		method: "GET",
		data: {username: username, password: password}
    });

    if(result.responseText.localeCompare("ERROR") || result.responseText.localeCompare("null")) {
		return false;
	}
	return true;
}

function destroySession(async) {
	$.ajax({
		url: "Backend/session", 
		async: async,
		method: "DELETE"
    });
}