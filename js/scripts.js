// Next and previous buttons
$( document ).ready(function() {
		$("#next").click(function () {
		   impress().next();
		});

		$("#prev").click(function () {
		   impress().prev();
		});
});

//Table of contents
$( document ).ready(function() {
		var links = "<nav role='navigation' class='table-of-contents'>" +
			"<h3 class='h3'>Jump to Slide:</h3>" +
			"<ul class='navlinks'>";

		var menuItem, el, title, link;

		$(".step").each(function() {
			el = $(this);
			title = el.attr("title");
			link = "#" + el.attr("id");

			menuItem = "<li><a href=" + link + ">" + title + "</a></li>";

			links += menuItem;
		});

		links += "</ul></nav>";

		$(".contents").prepend(links);
});

// Open/close ToC
$( document ).ready(function() {
	$(".contents-menu").click(function() {
		$('body').toggleClass("menu-active");
	});
});