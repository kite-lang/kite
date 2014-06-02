var graphs = ['parse_tree'];

function loadContent(graph) {
    if (graphs.indexOf(graph) < 0) {
        throw graph + " is not a graph."
    }

    $('.main').html('')
    if (graph === 'home') {
        $('.main').load('src/home.html');
        return;
    }

    var file = 'src/' + graph + '.js';
    $.getScript(file);
}

$(loadContent("parse_tree"))
