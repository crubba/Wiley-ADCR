$("#quoteButton").click(function(){
    $.getJSON("quotes/all_quotes.json", function(data){
	$.each(data, function(key, value){
	    $("body").prepend("<div date='"+value.date+"'><h1>"+value.author+"</h1><p><i>'"+value.quote+"'</i></p><p><b>Source: </b>'"+value.source+"'</p></div>");
	});
    });
});