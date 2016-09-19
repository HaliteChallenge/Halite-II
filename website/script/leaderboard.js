$(function() {

	var field = getGET("field");
	var value = getGET("value");
	var heading = getGET("heading");
	var $heading = $("#leaderHeading");

	if(field != null && value != null && heading != null) {
		$heading.html(heading + " Rankings");

		var filters = {};
		filters["status"] = 3;
		filters[field] = value;
		console.log(filters)
		leaderTable.init(getFilteredUsers(filters, "rank"));
	} else {
		$heading.html("Current Rankings");
		leaderTable.init(getActiveUsers());
	}
})
