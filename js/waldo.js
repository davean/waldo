// Note: jQuery should be included before this script.
var ENDPOINT = 'http://127.0.0.1:3000/story/jarUcyikAg3',
    PANELPATH = 'loadedPanels/'

function renderComic(comicScript) {
    var $comic = $('<div>')
        .addClass('comic')
        .css({
            width: comicScript.width,
            height: comicScript.height
        })
    $.each(comicScript.panels, function(idx, panel) {
        var $panel = $('<div>')
            .addClass('panel')
            .css({
                left: panel.pos.x,
                top: panel.pos.y,
                width: panel.width,
                height: panel.height
            })
            .appendTo($comic)
        $.each(panel.images, function(idx, image) {
            var $img = $('<img>')
                .css({
                    left: image.pos.x,
                    top: image.pos.y
                })
                .attr('src', PANELPATH + image.url)
                .appendTo($panel)
        })
	    })
	var $cover = $('<div>')
        .addClass('cover')
        .css({
            left: 0,
            top: 0,
            width: $comic.width(),
            height: $comic.height()
        })
        .attr('title', comicScript.alttext)
        .appendTo($comic)
    return $comic
}

comicHandler = {
    lastComic: null,
    fetchComic: function() {
        var details = {
            w: $(window).width(),
            h: $(window).height(),
            r: document.referrer
        }

        var sidePadding = 10
        $.ajax(ENDPOINT, {
            data: details,
            dataType: 'jsonp',
            jsonpCallback: 'waldoCallback',
            success: $.proxy(function(comicScript) {
                if (comicScript.goto) {
                    window.location = comicScript.goto
                }

                var comic = renderComic(comicScript),
                comicWidth = comic.outerWidth(true),
                comicHeight = comic.height()

                $('#container')
                    .empty()
                    .append(comic)
                    .width(comicWidth)
                    .height(comicHeight)

                this.lastComic = comicScript
            }, this)
        })
    }
}

resizeHandler = {
    delay: 250,
    timeout: null,
    onResize: function() {
        if (!this.timeout) {
            this.timeout = setTimeout($.proxy(function() {
                this.timeout = null
                comicHandler.fetchComic()
            }, this), this.delay)
        }
    }
}

$(document).ready($.proxy(comicHandler, 'fetchComic'))
$(window).resize(function() {
    resizeHandler.onResize()
})
