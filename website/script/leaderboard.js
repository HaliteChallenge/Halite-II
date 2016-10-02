$(function() {
    var field = getGET("field");
    var value = getGET("value");
    var heading = getGET("heading");
    var page = getGET("page");

    var $heading = $("#leaderHeading");

	var filters = {};
	filters["isRunning"] = 1;
    if(field != null && value != null && heading != null) {
        $heading.html(heading + " Rankings");
		filters[field] = value;
    } else {
        $heading.html("Current Rankings");
    }

	var response = getFilteredUsers(filters, "rank", 50, page)
	console.log(response)
	leaderTable.init(response['users']);

    var $previous = null;
    var $next = null;

	var baseURL = location.pathname.substring(location.pathname.lastIndexOf("/") + 1)+"?field="+field+"&value="+value+"&heading="+heading;
	if(page != null && page != 0) {
		$previous = $("<a/>");
		$previous.attr("href", baseURL+"&page="+(page-1));
	} else {
		$previous = $("<span/>");
		$previous.addClass('text-muted');
	}

	if(response['isNextPage'] == true) {
		$next = $("<a/>");
		$next.attr("href", baseURL+"&page="+(page == null ? 1 : page+1));
	} else {
		$next = $("<span/>");
		$next.addClass('text-muted');
	}

	$previous.addClass("pull-left");
	$previous.html("Previous");
	$next.addClass("pull-right");
	$next.html("Next");

	$footer = $("#footer");
	$footer.append($previous);
	$footer.append($next);

})
