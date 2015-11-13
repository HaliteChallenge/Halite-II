// Attempts to store the username/password combo given
// Returns false if the username is already taken
// If async returns null
function putUsernamePasswordDatabase(username, password, async) {
	var result = $.ajax({
		url: "Backend/user", 
		async: async,
		method: "POST",
		data: {username: username, password: password}
    });

    if(async == true) {
    	return null;
    }

    return didSucceed(result);
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
	if(didSucceed(response) == false)  return null;
	return response.responseJSON;
}

function getUserCredentials(username, password) {
	var result = $.ajax({
		url: "Backend/user", 
		async: false,
		method: "GET",
		data: {username: username, password: password}
    });

    if(didSucceed(response) == false)  return null;
	return result.responseJSON;
}

function destroySession(async) {
	$.ajax({
		url: "Backend/session", 
		async: async,
		method: "DELETE"
    });
}

function didSucceed(response) {
	if(response.responseText.localeCompare("null") == 0 || Object.keys(response.responseJSON).length === 0) {
		return false;
	}
	return true;
}