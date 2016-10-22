$(function() {
    var USERS_PER_PAGE = 50;

    // What criteria are we using for ranking?
    var field = getGET("field");
    var value = getGET("value");
    var heading = getGET("heading");
    
    // Where are we in the rankings?
    var userID = getGET("userID");
    var page = getGET("page");
    if(page == null) {
        if (userID != null) page = Math.floor((parseInt(getUser(userID)["rank"]) - 1) / USERS_PER_PAGE);
        else page = 0;
    } else {
        page = parseInt(page);
    }

    // Create leaderboard filters from get params
    // Set page title 
    var $heading = $("#leaderHeading");
    var filters = {};
    filters["isRunning"] = 1;
    if(field != null && value != null && heading != null) {
        $heading.html(heading + " Rankings");
        document.title = heading + " Rankings";
        filters[field] = value;
    } else {
        $heading.html("Current Rankings");
    }

    var response = getFilteredUsers(filters, "rank", USERS_PER_PAGE, page);
    leaderTable.init(response['users'], 1+(page == null ? 0 : page)*USERS_PER_PAGE);

    // Highlight if user was linked
    if(userID != null) {
        var $username = leaderTable.getRow(userID).find(".username");
        $username.html("<mark>"+$username.html()+"</mark>");
    }

    // Create next and previous buttons
    // (build links, are they greyed out, etc)
    var $previous = null;
    var $next = null;

    var baseURL = location.pathname.substring(location.pathname.lastIndexOf("/") + 1)+"?";
    if(field != null) baseURL += "field="+field;
    if(value != null) baseURL += "&value="+value;
    if(heading != null) baseURL += "&heading="+heading;

    if(page != 0) {
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
