#lang scribble/html
@(require
    racket/port
    racket/format
    racket/file
    racket/vector
    scribble/html/extra
    "helpers.rkt")
@(define (a+img src) @a[target: "_blank" href: (~a "images/" src)]{@img[src: (~a "images/" src)]})
@doctype['html]
@html[lang: "en"]{
  @head{
    @meta[charset: "utf-8"]
      @meta[http-equiv: "X-UA-Compatible" content: "IE=edge"]
      @meta[name: "viewport" content: "width=device-width, initial-scale=1"]
      @title{Xenia, the Linux mascot}
    @link[rel: "stylesheet" href: "style.css"]
  }
  @body{
    @h1{Xenia, the Linux mascot}
    @element/not-empty['flag class: "b"]{}
    @element/not-empty['flag class: "p"]{}
    @element/not-empty['flag class: "w"]{}
    @element/not-empty['flag class: "p"]{}
    @element/not-empty['flag class: "b"]{}
    @br
    @center{
      @div[class: "webkit"]{@element['warn class: "r"]{⚠️}@element['warn class: "l"]{⚠️}Xenia suggests you use @a[href: "https://www.mozilla.org/en-US/firefox/new/" title: "🔥🦊"]{Firefox} for better safety and comfort.}
      @iframe[width: 560 height: 315 src: "https://www.youtube-nocookie.com/embed/0b4eW1KAuWE" frameborder: 0 allow: "accelerometer; autoplay: clipboard-write; encrypted-media; gyroscope; picture-in-picture;" allowfullscreen: "true"]
      @br
      Thanks to Immy for this awesome video.}
    @br
    @div[class: "story"]{
      @div[class: "left"]{Xenia, the fox girl mascot of @a[href: "https://www.linux.org/"]{Linux}, was first designed in 1996 by @a[href: "https://web.archive.org/web/20060610112303/http://www.almackey.com/"]{Alan Mackey}.
        She was meant to be @a[href: "https://archive.is/wYsFh"]{an alternative} to @a[href: "https://en.wikipedia.org/wiki/Tux_%28mascot%29"]{Tux}, the official mascot. Something that would resurface in @a[href: "https://itsfoss.com/tux-trivia/"]{an article} years later.}
      @div[class: "right"]{
        @a+img["linuxfox.gif"]
        @a+img["linuxfox_smltr.gif"]}}
    @div[class: "story"]{
      @div[class: "left"]{But far from being the end of the story, when twitter user @a[href: "https://twitter.com/cathodegaytube/"]{Cathodegaytube} found about this from her girlfriend in 2019, she decided to redraw her and @a[href: "https://twitter.com/cathodegaytube/status/1197227512075411456"]{posted about it}.}
      @div[class: "right"]{
        @a+img["cathodegaytube1.jpg"]}}
    @div[class: "story"]{
      @div[class: "left"]{Months later, in 2020, @a[href: "https://twitter.com/cathodegaytube/status/1269405917700710400"]{she revealed} how Alan Mackey reacted to her redesign.
        It was a surprise to her that Alan had first intended to make his fox a boy.}
      @div[class: "right"]{
        @a+img["trans-rights.jpeg"]}}
    @div[class: "story"]{
      @div[class: "left"]{As it happens, Xenia (a name chosen by @a[href: "https://twitter.com/cathodegaytube/status/1269409552027377664"]{Cathodegaytube's girlfriend}, @a[href: "https://twitter.com/AmyWrightDev"]{AmyWrightDev}), became a girl off-screen and came out during pride month.
        A series of coincidences brought about trans representation to this historically cishet male centric sector of software.
        Special thanks to @a[href: "https://archive.is/Ho4Md"]{this recap} on Mastodon (cybre.space) without which this page wouldn't be possible.@comment{archive https://web.archive.org/web/20220628062928/https://cybre.space/@"@"Balina/104299903634507945}}
      @div[class: "right"]{
        @a+img["email1.png"]
        @br
        @a+img["email2.png"]}}
    @div[class: "story"]{
      @div[class: "left"]{These news gave new life to Xenia and as artists became interested in her we saw some @a[href: "https://archive.is/xeJ39"]{more art} of her.}}
    @(custom-parse "data.txt")
    @div[style: "margin-left: 2em;"]{
      @element/not-empty['flag class: "b"]{}
      @element/not-empty['flag class: "p"]{}
      @element/not-empty['flag class: "w"]{}
      @element/not-empty['flag class: "p"]{}
      @element/not-empty['flag class: "b"]{}
    }
    @br
    @footer{
      @div[class: "left"]{If you make or find any other art of Xenia, please do @a[href: "https://chitter.xyz/@efi"]{contact me} and I will upload it here with full credit. Thanks!}
      @div[class: "right"]{The images in this site belong to their respective authors and are stored here for archival purposes, but the license of the character itself is CC0.
        @literal{<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://xenia-linux-site.glitch.me/">Xenia</a> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://web.archive.org/web/19970517213342/www.early.com/~emackey/linux/">Alan Mackey</a> is marked with <a href="http://creativecommons.org/publicdomain/zero/1.0?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC0 1.0 Universal<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/zero.svg?ref=chooser-v1"></a></p>}
          Website made with @a[href: "https://glitch.com"]{Glitch}!
      }
    }
    @div[class: "glitchButton" style: "position:fixed;top:20px;right:20px;"]
    @script[src: "https://button.glitch.me/button.js" defer: #t]
    @script[defer: #t]{
        const UA = navigator.userAgent;
        const EW = /WebKit/.test(UA) || /Trident/.test(UA);
        if (!EW) {
            document.querySelector(@"'.webkit'").setAttribute(@"'disabled'", true)
          }
      }
  }
}
