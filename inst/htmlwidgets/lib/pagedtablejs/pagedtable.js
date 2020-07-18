// Production steps of ECMA-262, Edition 5, 15.4.4.18
// Reference: http://es5.github.io/#x15.4.4.18
if (!Array.prototype.forEach) {

  Array.prototype.forEach = function(callback, thisArg) {

    var T, k;

    if (this === null) {
      throw new TypeError(' this is null or not defined');
    }

    // 1. Let O be the result of calling toObject() passing the
    // |this| value as the argument.
    var O = Object(this);

    // 2. Let lenValue be the result of calling the Get() internal
    // method of O with the argument "length".
    // 3. Let len be toUint32(lenValue).
    var len = O.length >>> 0;

    // 4. If isCallable(callback) is false, throw a TypeError exception.
    // See: http://es5.github.com/#x9.11
    if (typeof callback !== "function") {
      throw new TypeError(callback + ' is not a function');
    }

    // 5. If thisArg was supplied, let T be thisArg; else let
    // T be undefined.
    if (arguments.length > 1) {
      T = thisArg;
    }

    // 6. Let k be 0
    k = 0;

    // 7. Repeat, while k < len
    while (k < len) {

      var kValue;

      // a. Let Pk be ToString(k).
      //    This is implicit for LHS operands of the in operator
      // b. Let kPresent be the result of calling the HasProperty
      //    internal method of O with argument Pk.
      //    This step can be combined with c
      // c. If kPresent is true, then
      if (k in O) {

        // i. Let kValue be the result of calling the Get internal
        // method of O with argument Pk.
        kValue = O[k];

        // ii. Call the Call internal method of callback with T as
        // the this value and argument list containing kValue, k, and O.
        callback.call(T, kValue, k, O);
      }
      // d. Increase k by 1.
      k++;
    }
    // 8. return undefined
  };
}

// Production steps of ECMA-262, Edition 5, 15.4.4.19
// Reference: http://es5.github.io/#x15.4.4.19
if (!Array.prototype.map) {

  Array.prototype.map = function(callback, thisArg) {

    var T, A, k;

    if (this == null) {
      throw new TypeError(' this is null or not defined');
    }

    // 1. Let O be the result of calling ToObject passing the |this|
    //    value as the argument.
    var O = Object(this);

    // 2. Let lenValue be the result of calling the Get internal
    //    method of O with the argument "length".
    // 3. Let len be ToUint32(lenValue).
    var len = O.length >>> 0;

    // 4. If IsCallable(callback) is false, throw a TypeError exception.
    // See: http://es5.github.com/#x9.11
    if (typeof callback !== 'function') {
      throw new TypeError(callback + ' is not a function');
    }

    // 5. If thisArg was supplied, let T be thisArg; else let T be undefined.
    if (arguments.length > 1) {
      T = thisArg;
    }

    // 6. Let A be a new array created as if by the expression new Array(len)
    //    where Array is the standard built-in constructor with that name and
    //    len is the value of len.
    A = new Array(len);

    // 7. Let k be 0
    k = 0;

    // 8. Repeat, while k < len
    while (k < len) {

      var kValue, mappedValue;

      // a. Let Pk be ToString(k).
      //   This is implicit for LHS operands of the in operator
      // b. Let kPresent be the result of calling the HasProperty internal
      //    method of O with argument Pk.
      //   This step can be combined with c
      // c. If kPresent is true, then
      if (k in O) {

        // i. Let kValue be the result of calling the Get internal
        //    method of O with argument Pk.
        kValue = O[k];

        // ii. Let mappedValue be the result of calling the Call internal
        //     method of callback with T as the this value and argument
        //     list containing kValue, k, and O.
        mappedValue = callback.call(T, kValue, k, O);

        // iii. Call the DefineOwnProperty internal method of A with arguments
        // Pk, Property Descriptor
        // { Value: mappedValue,
        //   Writable: true,
        //   Enumerable: true,
        //   Configurable: true },
        // and false.

        // In browsers that support Object.defineProperty, use the following:
        // Object.defineProperty(A, k, {
        //   value: mappedValue,
        //   writable: true,
        //   enumerable: true,
        //   configurable: true
        // });

        // For best browser support, use the following:
        A[k] = mappedValue;
      }
      // d. Increase k by 1.
      k++;
    }

    // 9. return A
    return A;
  };
}

/*! @license DOMPurify | (c) Cure53 and other contributors | Released under the Apache license 2.0 and Mozilla Public License 2.0 | github.com/cure53/DOMPurify/blob/2.0.8/LICENSE */
!function(e,t){"object"==typeof exports&&"undefined"!=typeof module?module.exports=t():"function"==typeof define&&define.amd?define(t):(e=e||self).DOMPurify=t()}(this,(function(){"use strict";var e=Object.hasOwnProperty,t=Object.setPrototypeOf,n=Object.isFrozen,r=Object.keys,o=Object.freeze,i=Object.seal,a="undefined"!=typeof Reflect&&Reflect,l=a.apply,c=a.construct;l||(l=function(e,t,n){return e.apply(t,n)}),o||(o=function(e){return e}),i||(i=function(e){return e}),c||(c=function(e,t){return new(Function.prototype.bind.apply(e,[null].concat(function(e){if(Array.isArray(e)){for(var t=0,n=Array(e.length);t<e.length;t++)n[t]=e[t];return n}return Array.from(e)}(t))))});var s=S(Array.prototype.forEach),u=S(Array.prototype.indexOf),d=S(Array.prototype.join),f=S(Array.prototype.pop),p=S(Array.prototype.push),m=S(Array.prototype.slice),y=S(String.prototype.toLowerCase),g=S(String.prototype.match),h=S(String.prototype.replace),v=S(String.prototype.indexOf),b=S(String.prototype.trim),T=S(RegExp.prototype.test),A=k(RegExp),x=k(TypeError);function S(e){return function(t){for(var n=arguments.length,r=Array(n>1?n-1:0),o=1;o<n;o++)r[o-1]=arguments[o];return l(e,t,r)}}function k(e){return function(){for(var t=arguments.length,n=Array(t),r=0;r<t;r++)n[r]=arguments[r];return c(e,n)}}function L(e,r){t&&t(e,null);for(var o=r.length;o--;){var i=r[o];if("string"==typeof i){var a=y(i);a!==i&&(n(r)||(r[o]=a),i=a)}e[i]=!0}return e}function _(t){var n={},r=void 0;for(r in t)l(e,t,[r])&&(n[r]=t[r]);return n}var E=o(["a","abbr","acronym","address","area","article","aside","audio","b","bdi","bdo","big","blink","blockquote","body","br","button","canvas","caption","center","cite","code","col","colgroup","content","data","datalist","dd","decorator","del","details","dfn","dir","div","dl","dt","element","em","fieldset","figcaption","figure","font","footer","form","h1","h2","h3","h4","h5","h6","head","header","hgroup","hr","html","i","img","input","ins","kbd","label","legend","li","main","map","mark","marquee","menu","menuitem","meter","nav","nobr","ol","optgroup","option","output","p","picture","pre","progress","q","rp","rt","ruby","s","samp","section","select","shadow","small","source","spacer","span","strike","strong","style","sub","summary","sup","table","tbody","td","template","textarea","tfoot","th","thead","time","tr","track","tt","u","ul","var","video","wbr"]),M=o(["svg","a","altglyph","altglyphdef","altglyphitem","animatecolor","animatemotion","animatetransform","audio","canvas","circle","clippath","defs","desc","ellipse","filter","font","g","glyph","glyphref","hkern","image","line","lineargradient","marker","mask","metadata","mpath","path","pattern","polygon","polyline","radialgradient","rect","stop","style","switch","symbol","text","textpath","title","tref","tspan","video","view","vkern"]),D=o(["feBlend","feColorMatrix","feComponentTransfer","feComposite","feConvolveMatrix","feDiffuseLighting","feDisplacementMap","feDistantLight","feFlood","feFuncA","feFuncB","feFuncG","feFuncR","feGaussianBlur","feMerge","feMergeNode","feMorphology","feOffset","fePointLight","feSpecularLighting","feSpotLight","feTile","feTurbulence"]),R=o(["math","menclose","merror","mfenced","mfrac","mglyph","mi","mlabeledtr","mmultiscripts","mn","mo","mover","mpadded","mphantom","mroot","mrow","ms","mspace","msqrt","mstyle","msub","msup","msubsup","mtable","mtd","mtext","mtr","munder","munderover"]),N=o(["#text"]),O=o(["accept","action","align","alt","autocapitalize","autocomplete","autopictureinpicture","autoplay","background","bgcolor","border","capture","cellpadding","cellspacing","checked","cite","class","clear","color","cols","colspan","controls","controlslist","coords","crossorigin","datetime","decoding","default","dir","disabled","disablepictureinpicture","disableremoteplayback","download","draggable","enctype","enterkeyhint","face","for","headers","height","hidden","high","href","hreflang","id","inputmode","integrity","ismap","kind","label","lang","list","loading","loop","low","max","maxlength","media","method","min","minlength","multiple","muted","name","noshade","novalidate","nowrap","open","optimum","pattern","placeholder","playsinline","poster","preload","pubdate","radiogroup","readonly","rel","required","rev","reversed","role","rows","rowspan","spellcheck","scope","selected","shape","size","sizes","span","srclang","start","src","srcset","step","style","summary","tabindex","title","translate","type","usemap","valign","value","width","xmlns"]),w=o(["accent-height","accumulate","additive","alignment-baseline","ascent","attributename","attributetype","azimuth","basefrequency","baseline-shift","begin","bias","by","class","clip","clip-path","clip-rule","color","color-interpolation","color-interpolation-filters","color-profile","color-rendering","cx","cy","d","dx","dy","diffuseconstant","direction","display","divisor","dur","edgemode","elevation","end","fill","fill-opacity","fill-rule","filter","filterunits","flood-color","flood-opacity","font-family","font-size","font-size-adjust","font-stretch","font-style","font-variant","font-weight","fx","fy","g1","g2","glyph-name","glyphref","gradientunits","gradienttransform","height","href","id","image-rendering","in","in2","k","k1","k2","k3","k4","kerning","keypoints","keysplines","keytimes","lang","lengthadjust","letter-spacing","kernelmatrix","kernelunitlength","lighting-color","local","marker-end","marker-mid","marker-start","markerheight","markerunits","markerwidth","maskcontentunits","maskunits","max","mask","media","method","mode","min","name","numoctaves","offset","operator","opacity","order","orient","orientation","origin","overflow","paint-order","path","pathlength","patterncontentunits","patterntransform","patternunits","points","preservealpha","preserveaspectratio","primitiveunits","r","rx","ry","radius","refx","refy","repeatcount","repeatdur","restart","result","rotate","scale","seed","shape-rendering","specularconstant","specularexponent","spreadmethod","startoffset","stddeviation","stitchtiles","stop-color","stop-opacity","stroke-dasharray","stroke-dashoffset","stroke-linecap","stroke-linejoin","stroke-miterlimit","stroke-opacity","stroke","stroke-width","style","surfacescale","tabindex","targetx","targety","transform","text-anchor","text-decoration","text-rendering","textlength","type","u1","u2","unicode","values","viewbox","visibility","version","vert-adv-y","vert-origin-x","vert-origin-y","width","word-spacing","wrap","writing-mode","xchannelselector","ychannelselector","x","x1","x2","xmlns","y","y1","y2","z","zoomandpan"]),H=o(["accent","accentunder","align","bevelled","close","columnsalign","columnlines","columnspan","denomalign","depth","dir","display","displaystyle","encoding","fence","frame","height","href","id","largeop","length","linethickness","lspace","lquote","mathbackground","mathcolor","mathsize","mathvariant","maxsize","minsize","movablelimits","notation","numalign","open","rowalign","rowlines","rowspacing","rowspan","rspace","rquote","scriptlevel","scriptminsize","scriptsizemultiplier","selection","separator","separators","stretchy","subscriptshift","supscriptshift","symmetric","voffset","width","xmlns"]),C=o(["xlink:href","xml:id","xlink:title","xml:space","xmlns:xlink"]),F=i(/\{\{[\s\S]*|[\s\S]*\}\}/gm),z=i(/<%[\s\S]*|[\s\S]*%>/gm),I=i(/^data-[\-\w.\u00B7-\uFFFF]/),U=i(/^aria-[\-\w]+$/),j=i(/^(?:(?:(?:f|ht)tps?|mailto|tel|callto|cid|xmpp):|[^a-z]|[a-z+.\-]+(?:[^a-z+.\-:]|$))/i),P=i(/^(?:\w+script|data):/i),G=i(/[\u0000-\u0020\u00A0\u1680\u180E\u2000-\u2029\u205f\u3000]/g),W="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(e){return typeof e}:function(e){return e&&"function"==typeof Symbol&&e.constructor===Symbol&&e!==Symbol.prototype?"symbol":typeof e};function B(e){if(Array.isArray(e)){for(var t=0,n=Array(e.length);t<e.length;t++)n[t]=e[t];return n}return Array.from(e)}var q=function(){return"undefined"==typeof window?null:window},K=function(e,t){if("object"!==(void 0===e?"undefined":W(e))||"function"!=typeof e.createPolicy)return null;var n=null;t.currentScript&&t.currentScript.hasAttribute("data-tt-policy-suffix")&&(n=t.currentScript.getAttribute("data-tt-policy-suffix"));var r="dompurify"+(n?"#"+n:"");try{return e.createPolicy(r,{createHTML:function(e){return e}})}catch(e){return console.warn("TrustedTypes policy "+r+" could not be created."),null}};return function e(){var t=arguments.length>0&&void 0!==arguments[0]?arguments[0]:q(),n=function(t){return e(t)};if(n.version="2.0.12",n.removed=[],!t||!t.document||9!==t.document.nodeType)return n.isSupported=!1,n;var i=t.document,a=!1,l=t.document,c=t.DocumentFragment,S=t.HTMLTemplateElement,k=t.Node,V=t.NodeFilter,Y=t.NamedNodeMap,X=void 0===Y?t.NamedNodeMap||t.MozNamedAttrMap:Y,$=t.Text,J=t.Comment,Q=t.DOMParser,Z=t.trustedTypes;if("function"==typeof S){var ee=l.createElement("template");ee.content&&ee.content.ownerDocument&&(l=ee.content.ownerDocument)}var te=K(Z,i),ne=te&&He?te.createHTML(""):"",re=l,oe=re.implementation,ie=re.createNodeIterator,ae=re.getElementsByTagName,le=re.createDocumentFragment,ce=i.importNode,se={};n.isSupported=oe&&void 0!==oe.createHTMLDocument&&9!==l.documentMode;var ue=F,de=z,fe=I,pe=U,me=P,ye=G,ge=j,he=null,ve=L({},[].concat(B(E),B(M),B(D),B(R),B(N))),be=null,Te=L({},[].concat(B(O),B(w),B(H),B(C))),Ae=null,xe=null,Se=!0,ke=!0,Le=!1,_e=!1,Ee=!1,Me=!1,De=!1,Re=!1,Ne=!1,Oe=!1,we=!1,He=!1,Ce=!0,Fe=!0,ze=!1,Ie={},Ue=L({},["annotation-xml","audio","colgroup","desc","foreignobject","head","iframe","math","mi","mn","mo","ms","mtext","noembed","noframes","plaintext","script","style","svg","template","thead","title","video","xmp"]),je=null,Pe=L({},["audio","video","img","source","image","track"]),Ge=null,We=L({},["alt","class","for","id","label","name","pattern","placeholder","summary","title","value","style","xmlns"]),Be=null,qe=l.createElement("form"),Ke=function(e){Be&&Be===e||(e&&"object"===(void 0===e?"undefined":W(e))||(e={}),he="ALLOWED_TAGS"in e?L({},e.ALLOWED_TAGS):ve,be="ALLOWED_ATTR"in e?L({},e.ALLOWED_ATTR):Te,Ge="ADD_URI_SAFE_ATTR"in e?L(_(We),e.ADD_URI_SAFE_ATTR):We,je="ADD_DATA_URI_TAGS"in e?L(_(Pe),e.ADD_DATA_URI_TAGS):Pe,Ae="FORBID_TAGS"in e?L({},e.FORBID_TAGS):{},xe="FORBID_ATTR"in e?L({},e.FORBID_ATTR):{},Ie="USE_PROFILES"in e&&e.USE_PROFILES,Se=!1!==e.ALLOW_ARIA_ATTR,ke=!1!==e.ALLOW_DATA_ATTR,Le=e.ALLOW_UNKNOWN_PROTOCOLS||!1,_e=e.SAFE_FOR_JQUERY||!1,Ee=e.SAFE_FOR_TEMPLATES||!1,Me=e.WHOLE_DOCUMENT||!1,Ne=e.RETURN_DOM||!1,Oe=e.RETURN_DOM_FRAGMENT||!1,we=e.RETURN_DOM_IMPORT||!1,He=e.RETURN_TRUSTED_TYPE||!1,Re=e.FORCE_BODY||!1,Ce=!1!==e.SANITIZE_DOM,Fe=!1!==e.KEEP_CONTENT,ze=e.IN_PLACE||!1,ge=e.ALLOWED_URI_REGEXP||ge,Ee&&(ke=!1),Oe&&(Ne=!0),Ie&&(he=L({},[].concat(B(N))),be=[],!0===Ie.html&&(L(he,E),L(be,O)),!0===Ie.svg&&(L(he,M),L(be,w),L(be,C)),!0===Ie.svgFilters&&(L(he,D),L(be,w),L(be,C)),!0===Ie.mathMl&&(L(he,R),L(be,H),L(be,C))),e.ADD_TAGS&&(he===ve&&(he=_(he)),L(he,e.ADD_TAGS)),e.ADD_ATTR&&(be===Te&&(be=_(be)),L(be,e.ADD_ATTR)),e.ADD_URI_SAFE_ATTR&&L(Ge,e.ADD_URI_SAFE_ATTR),Fe&&(he["#text"]=!0),Me&&L(he,["html","head","body"]),he.table&&(L(he,["tbody"]),delete Ae.tbody),o&&o(e),Be=e)},Ve=function(e){p(n.removed,{element:e});try{e.parentNode.removeChild(e)}catch(t){e.outerHTML=ne}},Ye=function(e,t){try{p(n.removed,{attribute:t.getAttributeNode(e),from:t})}catch(e){p(n.removed,{attribute:null,from:t})}t.removeAttribute(e)},Xe=function(e){var t=void 0,n=void 0;if(Re)e="<remove></remove>"+e;else{var r=g(e,/^[\r\n\t ]+/);n=r&&r[0]}var o=te?te.createHTML(e):e;try{t=(new Q).parseFromString(o,"text/html")}catch(e){}if(a&&L(Ae,["title"]),!t||!t.documentElement){var i=(t=oe.createHTMLDocument("")).body;i.parentNode.removeChild(i.parentNode.firstElementChild),i.outerHTML=o}return e&&n&&t.body.insertBefore(l.createTextNode(n),t.body.childNodes[0]||null),ae.call(t,Me?"html":"body")[0]};n.isSupported&&function(){try{var e=Xe("<x/><title>&lt;/title&gt;&lt;img&gt;");T(/<\/title/,e.querySelector("title").innerHTML)&&(a=!0)}catch(e){}}();var $e=function(e){return ie.call(e.ownerDocument||e,e,V.SHOW_ELEMENT|V.SHOW_COMMENT|V.SHOW_TEXT,(function(){return V.FILTER_ACCEPT}),!1)},Je=function(e){return!(e instanceof $||e instanceof J)&&!("string"==typeof e.nodeName&&"string"==typeof e.textContent&&"function"==typeof e.removeChild&&e.attributes instanceof X&&"function"==typeof e.removeAttribute&&"function"==typeof e.setAttribute&&"string"==typeof e.namespaceURI)},Qe=function(e){return"object"===(void 0===k?"undefined":W(k))?e instanceof k:e&&"object"===(void 0===e?"undefined":W(e))&&"number"==typeof e.nodeType&&"string"==typeof e.nodeName},Ze=function(e,t,r){se[e]&&s(se[e],(function(e){e.call(n,t,r,Be)}))},et=function(e){var t=void 0;if(Ze("beforeSanitizeElements",e,null),Je(e))return Ve(e),!0;var r=y(e.nodeName);if(Ze("uponSanitizeElement",e,{tagName:r,allowedTags:he}),("svg"===r||"math"===r)&&0!==e.querySelectorAll("p, br").length)return Ve(e),!0;if(!he[r]||Ae[r]){if(Fe&&!Ue[r]&&"function"==typeof e.insertAdjacentHTML)try{var o=e.innerHTML;e.insertAdjacentHTML("AfterEnd",te?te.createHTML(o):o)}catch(e){}return Ve(e),!0}return"noscript"===r&&T(/<\/noscript/i,e.innerHTML)||"noembed"===r&&T(/<\/noembed/i,e.innerHTML)?(Ve(e),!0):(!_e||e.firstElementChild||e.content&&e.content.firstElementChild||!T(/</g,e.textContent)||(p(n.removed,{element:e.cloneNode()}),e.innerHTML?e.innerHTML=h(e.innerHTML,/</g,"&lt;"):e.innerHTML=h(e.textContent,/</g,"&lt;")),Ee&&3===e.nodeType&&(t=e.textContent,t=h(t,ue," "),t=h(t,de," "),e.textContent!==t&&(p(n.removed,{element:e.cloneNode()}),e.textContent=t)),Ze("afterSanitizeElements",e,null),!1)},tt=function(e,t,n){if(Ce&&("id"===t||"name"===t)&&(n in l||n in qe))return!1;if(ke&&T(fe,t));else if(Se&&T(pe,t));else{if(!be[t]||xe[t])return!1;if(Ge[t]);else if(T(ge,h(n,ye,"")));else if("src"!==t&&"xlink:href"!==t&&"href"!==t||"script"===e||0!==v(n,"data:")||!je[e]){if(Le&&!T(me,h(n,ye,"")));else if(n)return!1}else;}return!0},nt=function(e){var t=void 0,o=void 0,i=void 0,a=void 0,l=void 0;Ze("beforeSanitizeAttributes",e,null);var c=e.attributes;if(c){var s={attrName:"",attrValue:"",keepAttr:!0,allowedAttributes:be};for(l=c.length;l--;){var p=t=c[l],g=p.name,v=p.namespaceURI;if(o=b(t.value),i=y(g),s.attrName=i,s.attrValue=o,s.keepAttr=!0,s.forceKeepAttr=void 0,Ze("uponSanitizeAttribute",e,s),o=s.attrValue,!s.forceKeepAttr){if("name"===i&&"IMG"===e.nodeName&&c.id)a=c.id,c=m(c,[]),Ye("id",e),Ye(g,e),u(c,a)>l&&e.setAttribute("id",a.value);else{if("INPUT"===e.nodeName&&"type"===i&&"file"===o&&s.keepAttr&&(be[i]||!xe[i]))continue;"id"===g&&e.setAttribute(g,""),Ye(g,e)}if(s.keepAttr)if(_e&&T(/\/>/i,o))Ye(g,e);else if(T(/svg|math/i,e.namespaceURI)&&T(A("</("+d(r(Ue),"|")+")","i"),o))Ye(g,e);else{Ee&&(o=h(o,ue," "),o=h(o,de," "));var x=e.nodeName.toLowerCase();if(tt(x,i,o))try{v?e.setAttributeNS(v,g,o):e.setAttribute(g,o),f(n.removed)}catch(e){}}}}Ze("afterSanitizeAttributes",e,null)}},rt=function e(t){var n=void 0,r=$e(t);for(Ze("beforeSanitizeShadowDOM",t,null);n=r.nextNode();)Ze("uponSanitizeShadowNode",n,null),et(n)||(n.content instanceof c&&e(n.content),nt(n));Ze("afterSanitizeShadowDOM",t,null)};return n.sanitize=function(e,r){var o=void 0,a=void 0,l=void 0,s=void 0,u=void 0;if(e||(e="\x3c!--\x3e"),"string"!=typeof e&&!Qe(e)){if("function"!=typeof e.toString)throw x("toString is not a function");if("string"!=typeof(e=e.toString()))throw x("dirty is not a string, aborting")}if(!n.isSupported){if("object"===W(t.toStaticHTML)||"function"==typeof t.toStaticHTML){if("string"==typeof e)return t.toStaticHTML(e);if(Qe(e))return t.toStaticHTML(e.outerHTML)}return e}if(De||Ke(r),n.removed=[],"string"==typeof e&&(ze=!1),ze);else if(e instanceof k)1===(a=(o=Xe("\x3c!--\x3e")).ownerDocument.importNode(e,!0)).nodeType&&"BODY"===a.nodeName||"HTML"===a.nodeName?o=a:o.appendChild(a);else{if(!Ne&&!Ee&&!Me&&-1===e.indexOf("<"))return te&&He?te.createHTML(e):e;if(!(o=Xe(e)))return Ne?null:ne}o&&Re&&Ve(o.firstChild);for(var d=$e(ze?e:o);l=d.nextNode();)3===l.nodeType&&l===s||et(l)||(l.content instanceof c&&rt(l.content),nt(l),s=l);if(s=null,ze)return e;if(Ne){if(Oe)for(u=le.call(o.ownerDocument);o.firstChild;)u.appendChild(o.firstChild);else u=o;return we&&(u=ce.call(i,u,!0)),u}var f=Me?o.outerHTML:o.innerHTML;return Ee&&(f=h(f,ue," "),f=h(f,de," ")),te&&He?te.createHTML(f):f},n.setConfig=function(e){Ke(e),De=!0},n.clearConfig=function(){Be=null,De=!1},n.isValidAttribute=function(e,t,n){Be||Ke({});var r=y(e),o=y(t);return tt(r,o,n)},n.addHook=function(e,t){"function"==typeof t&&(se[e]=se[e]||[],p(se[e],t))},n.removeHook=function(e){se[e]&&f(se[e])},n.removeHooks=function(e){se[e]&&(se[e]=[])},n.removeAllHooks=function(){se={}},n}()}));
//# sourceMappingURL=purify.min.js.map

var pagedTableStyle = "\
.pagedtable {\
  overflow: none;\
  padding-left: 8px;\
  padding-right: 8px;\
  color: #333;\
  font-family: \"Helvetica Neue\",Helvetica,Arial,sans-serif;\
  font-size: 14px;\
}\
\
.pagedtable-wrapper {\
  border-radius: 4px;\
}\
\
.pagedtable table {\
  width: 100%;\
  max-width: 100%;\
  margin: 0;\
}\
\
.pagedtable th {\
  padding: 0 5px 3px 5px;\
  border: none;\
  border-bottom: 2px solid #dddddd;\
\
  min-width: 45px;\
}\
\
.pagedtable-empty th {\
  display: none;\
}\
\
.pagedtable td {\
  padding: 8px 8px 8px 8px;\
}\
\
.pagedtable .even {\
  background-color: rgba(140, 140, 140, 0.1);\
}\
\
.pagedtable-padding-col {\
  display: none;\
}\
\
.pagedtable a {\
  -webkit-touch-callout: none;\
  -webkit-user-select: none;\
  -khtml-user-select: none;\
  -moz-user-select: none;\
  -ms-user-select: none;\
  user-select: none;\
}\
\
.pagedtable-index-nav {\
  cursor: pointer;\
  padding: 0 5px 0 5px;\
  float: right;\
  border: 0;\
}\
\
.pagedtable-index-nav-disabled {\
  cursor: default;\
  text-decoration: none;\
  color: #999;\
}\
\
a.pagedtable-index-nav-disabled:hover {\
  text-decoration: none;\
  color: #999;\
}\
\
.pagedtable-indexes {\
  cursor: pointer;\
  float: right;\
  border: 0;\
}\
\
.pagedtable-index-current {\
  cursor: default;\
  text-decoration: none;\
  font-weight: bold;\
  color: #333;\
  border: 0;\
}\
\
a.pagedtable-index-current:hover {\
  text-decoration: none;\
  font-weight: bold;\
  color: #333;\
}\
\
.pagedtable-index {\
  width: 30px;\
  display: inline-block;\
  text-align: center;\
  border: 0;\
}\
\
.pagedtable-index-separator-left {\
  display: inline-block;\
  color: #333;\
  font-size: 9px;\
  padding: 0 0 0 0;\
  cursor: default;\
}\
\
.pagedtable-index-separator-right {\
  display: inline-block;\
  color: #333;\
  font-size: 9px;\
  padding: 0 4px 0 0;\
  cursor: default;\
}\
\
.pagedtable-footer {\
  padding-top: 4px;\
  padding-bottom: 5px;\
}\
\
.pagedtable-not-empty .pagedtable-footer {\
  border-top: 2px solid #dddddd;\
}\
\
.pagedtable-info {\
  overflow: hidden;\
  color: #999;\
  white-space: nowrap;\
  text-overflow: ellipsis;\
}\
\
.pagedtable-header-name {\
  overflow: hidden;\
  text-overflow: ellipsis;\
}\
\
.pagedtable-header-type {\
  color: #999;\
  font-weight: 400;\
}\
\
.pagedtable-na-cell {\
  font-style: italic;\
  opacity: 0.3;\
}\
";

var PagedTable = function (pagedTable, source) {

  //Setup functions
    var me = this;

  // immediately evaluate and extract/parse "source" w/ some error handling
  var source = function(pagedTable, source) {
    if (typeof(source) === "undefined") {
      var sourceElems = [].slice.call(pagedTable.children).filter(function(e) {
        return e.hasAttribute("data-pagedtable-source");
      });

      if (sourceElems === null || sourceElems.length !== 1) {
        throw("A single data-pagedtable-source was not found");
      }

      source = JSON.parse(sourceElems[0].innerHTML);
    }

    return source;
  }(pagedTable, source);

  // get the paged table element, apply styling using shadowDOM,
  var pagedTable = function(pagedTable, source) {
    if (typeof(pagedTable) === "string") {
      pagedTable = document.getElementById(pagedTable);
    }

    // custom style
    var appliedStyle = pagedTableStyle;
    var styleElems = [].slice.call(pagedTable.children).filter(function(e) {
      return e.nodeName === "STYLE";
    });
    if (styleElems !== null && styleElems.length === 1) {
      appliedStyle = appliedStyle + styleElems[0].innerHTML;
    }

    // create style
    var style = document.createElement('style');
    style.type = 'text/css';
    if (style.styleSheet) {
      style.styleSheet.cssText = appliedStyle; // IE8 and below.
    } else {
      style.appendChild(document.createTextNode(appliedStyle));
    }

    var useShadowDOM = typeof(source.options) === "undefined" ||
                       typeof(source.options.shadowDOM) === "undefined" || source.options.shadowDOM;
    useShadowDOM = (document.head.createShadowRoot || document.head.attachShadow) && useShadowDOM;
    if (useShadowDOM) {
      pagedTable = pagedTable.attachShadow({mode: 'open'});
      pagedTable.appendChild(style);
    }
    else {
      var head = document.head || document.getElementsByTagName('head')[0];
      head.appendChild(style);
    }

    return pagedTable;
  }(pagedTable, source);

  // update "source" with more details/columns
  source = function(source) {
    if (typeof(source.data) === "undefined") {
      source.data = source;
    }

    if (typeof(source.columns) === "undefined") {
      var columns = [];
      var keys = Object.keys(source.data[0]);
      for (var idx = 0; idx < keys.length; idx++) {
        columns[idx] = { name: keys[idx], html: false};
      }

      source.columns = columns;
    }
    // if the column contains an array of names, not dictionary
    else if (typeof(source.columns[0].length) !== "undefined") {
      var columns = []
      for (var idx = 0; idx < source.columns.length; idx++) {
        columns[idx] = { name: source.columns[idx], html: false };
      }

      source.columns = columns;
    }

    //populate columns with missing columns info

    var keys = Object.keys(source.data[0]);
    if(source.columns.length != keys.length){
      var existingKeys = []
      for (var idx = 0; idx < source.columns.length; idx++) {
          existingKeys.push(source.columns[idx].name);
      }
      var missingKeys = keys.filter(x => !existingKeys.includes(x))

      //populate with missing

      for( var idx = 0; idx < missingKeys.length; idx++) {
        source.columns.push({name: missingKeys[idx], html: false});
        existingKeys.push(missingKeys[idx]);
      }

      //preserve order of keys
      var columns = []
      for (var idx = 0; idx < source.columns.length; idx++) {
        columns[idx] = source.columns[existingKeys.indexOf(keys[idx])];
      }

      source.columns = columns;

    }

    //every column must have the label and the html designation
    for (var idx = 0; idx < source.data[0].length; idx ++) {
      if (typeof(source.columns[idx].html) === "undefined"){
        source.columns[idx].html = false;
      }
    }

    for (var idx = 0; idx < source.columns.length ; idx++) {
      if (typeof(source.columns[idx].label) === "undefined")
        source.columns[idx].label = source.columns[idx].name;
    }

    return source;
  }(source);

  // get and create options about the paged table.
  var options = function(source) {
    var options = typeof(source.options) !== "undefined" &&
      source.options !== null ? source.options : {};

    var columns = typeof(options.columns) !== "undefined" ? options.columns : {};
    var rows = typeof(options.rows) !== "undefined" ? options.rows : {};

    var positiveIntOrNull = function(value, def) {
      return parseInt(value) >= 0 ? parseInt(value) : def;
    };

    return {
      pages: positiveIntOrNull(options.pages, null),
      rows: {
        min: positiveIntOrNull(rows.min, 5),
        max: positiveIntOrNull(rows.max, 10),
        total: positiveIntOrNull(rows.total, null)
      },
      columns: {
        min: positiveIntOrNull(columns.min, 1),
        max: positiveIntOrNull(columns.max, 10),
        total: positiveIntOrNull(columns.total, null)
      }
    };
  }(source);

  // function that will convert a text entry (as is) into an html element or preserve it as text
  var makeCellContents = function(entry){
    var cellContents
    var sanitized = DOMPurify.sanitize(entry);

    if(sanitized.length === 0){
      cellContents = document.createTextNode(entry);
    } else {
      cellContents = sanitized;
    }
    return(cellContents);
  };

  // function that when invoked determines what the
  // average size of a text entry is to determine how wide to make text fields.
  var Measurer = function() {

    // set some default initial values that will get adjusted in runtime
    me.measures = {
      padding: 12,
      character: 8,
      height: 15,
      defaults: true
    };

    me.calculate = function(measuresCell) {
      if (!me.measures.defaults)
        return;

      var measuresCellStyle = window.getComputedStyle(measuresCell, null);

      var newPadding = parsePadding(measuresCellStyle.paddingLeft) +
            parsePadding(measuresCellStyle.paddingRight);

      var sampleString = "ABCDEFGHIJ0123456789";
      var newCharacter = Math.ceil(measuresCell.clientWidth / sampleString.length);

      if (newPadding <= 0 || newCharacter <= 0)
        return;

      me.measures.padding = newPadding;
      me.measures.character = newCharacter;
      me.measures.height = measuresCell.clientHeight;
      me.measures.defaults = false;
    };

    return me;
  };

  var Page = function(data, options) {
    var me = this;

    var defaults = {
      max: 7,
      rows: 10
    };

    var totalPages = function() {
      return Math.ceil(data.length / me.rows);
    };

    me.number = 0;
    me.max = options.pages !== null ? options.pages : defaults.max;
    me.visible = me.max;
    me.rows = options.rows.min !== null ? options.rows.min : defaults.rows;
    me.total = totalPages();

    me.setRows = function(newRows) {
      me.rows = newRows;
      me.total = totalPages();
    };

    me.setPageNumber = function(newPageNumber) {
      if (newPageNumber < 0) newPageNumber = 0;
      if (newPageNumber >= me.total) newPageNumber = me.total - 1;

      me.number = newPageNumber;
    };

    me.setVisiblePages = function(visiblePages) {
      me.visible = Math.min(me.max, visiblePages);
      me.setPageNumber(me.number);
    };

    me.getVisiblePageRange = function() {
      var start = me.number - Math.max(Math.floor((me.visible - 1) / 2), 0);
      var end = me.number + Math.floor(me.visible / 2) + 1;
      var pageCount = me.total;

      if (start < 0) {
        var diffToStart = 0 - start;
        start += diffToStart;
        end += diffToStart;
      }

      if (end > pageCount) {
        var diffToEnd = end - pageCount;
        start -= diffToEnd;
        end -= diffToEnd;
      }

      start = start < 0 ? 0 : start;
      end = end >= pageCount ? pageCount : end;

      var first = false;
      var last = false;

      if (start > 0 && me.visible > 1) {
        start = start + 1;
        first = true;
      }

      if (end < pageCount && me.visible > 2) {
        end = end - 1;
        last = true;
      }

      return {
        first: first,
        start: start,
        end: end,
        last: last
      };
    };

    me.getRowStart = function() {
      var rowStart = page.number * page.rows;
      if (rowStart < 0)
        rowStart = 0;

      return rowStart;
    };

    me.getRowEnd = function() {
      var rowStart = me.getRowStart();
      return rowStart + me.rows;
    };

    me.getRowEnd = function() {
      var rowStart = me.getRowStart();
      return rowStart + me.rows;
    };

    me.getVisRowEnd = function() {
      var rowStart = me.getRowStart();
      return Math.min(rowStart + me.rows, data.length);
    }

    me.getPaddingRows = function() {
      var rowStart = me.getRowStart();
      var rowEnd = me.getRowEnd();
      return data.length > me.rows ? me.rows - (rowEnd - rowStart) : 0;
    };

    me.getVisRows = function(){
      var visRowsPage = []
      for( var i = me.getRowStart(); i < me.getRowEnd(); i++){
        visRowsPage.push(i);
      }
      return visRowsPage
    }
  };

  var Columns = function(data, columns, options) {
    var me = this;

    me.defaults = {
      min: 5
    };

    me.number = 0;
    me.visible = columns.length ;
    me.total = columns.length;
    me.subset = [];
    me.padding = 0;
    me.min = options.columns.min !== null ? options.columns.min : me.defaults.min;
    me.max = options.columns.max !== null ? options.columns.max : null;
    me.widths = {};
    me.visCols = []

    var widthsLookAhead = Math.max(100, options.rows.min);
    var paddingColChars = 10;

    me.emptyNames = function() {
      columns.forEach(function(column) {
        if (columns.label !== null && columns.label !== "")
          return false;
      });

      return true;
    };

    var parsePadding = function(value) {
      return parseInt(value) >= 0 ? parseInt(value) : 0;
    };

    me.calculateWidths = function(measures) {
      // Here is where we want to precompute the html columns.
      columns.forEach(function(column) {
        var maxChars = Math.max(
          column.label.toString().length,
          column.type ? column.type.toString().length : 0
        );

        if(!column.html){
          for (var idxRow = 0; idxRow < Math.min(widthsLookAhead, data.length); idxRow++) {
            var content = data[idxRow][column.name.toString()];
            if (typeof(content) !== "string") content = content.toString();
            maxChars = Math.max(maxChars, content.length);
          }
        }

        me.widths[column.name] = {
          // width in characters
          chars: maxChars,
          // width for the inner html columns
          inner: maxChars * measures.character,
          // width adding outer styles like padding
          outer: maxChars * measures.character + measures.padding,
          // Is width based on internal html
          html: column.html
        };
      });
    };

    me.getWidth = function() {

      var widthOuter = 0;
      for (var idxCol = 0; idxCol < me.subset.length; idxCol++) {
        var columnName = me.subset[idxCol].name;
        widthOuter = widthOuter + me.widths[columnName].outer;
      }

      widthOuter = widthOuter + me.padding * paddingColChars * measurer.measures.character;

      if (me.hasMoreLeftColumns()) {
        widthOuter = widthOuter + columnNavigationWidthPX + measurer.measures.padding;
      }

      if (me.hasMoreRightColumns()) {
        widthOuter = widthOuter + columnNavigationWidthPX + measurer.measures.padding;
      }

      return widthOuter;
    };

    me.getColWidth = function(idx){
      var columnName = me.subset[idx].name;
      return([
        me.widths[columnName].outer,
        me.widths[columnName].html
      ]);
    }

    me.setColWidth = function(idx, width){
       me.subset[idx].outer = width;
    };

    me.setVisibleColumns = function(avail){
      me.visCols = avail
    };

    me.updateSlice = function() {
      if (me.number + me.visible >= me.total)
        me.number = me.total - me.visible;

      if (me.number < 0) me.number = 0;

      me.subset = columns.slice(me.number, Math.min(me.number + me.visible, me.total));

      me.subset = me.subset.map(function(column) {
        Object.keys(column).forEach(function(colKey) {
          column[colKey] = column[colKey] === null ? "" : column[colKey].toString();
        });

        column.width = null;
        return column;
      });
    };

    me.incColumnNumber = function(increment) {
      me.number = me.number + increment;
    };

    me.setColumnNumber = function(newNumber) {
      me.number = newNumber;
    };

    me.setPaddingCount = function(newPadding) {
      me.padding = newPadding;
    };

    me.getPaddingCount = function() {
      return me.padding;
    };

    me.hasMoreLeftColumns = function() {
      return me.number > 0;
    };

    me.hasMoreRightColumns = function() {
      return me.number + me.visible < me.total;
    };

    me.updateSlice(0);
    return me;
  };

  // Start evaluating the data
  var data = source.data;
  var page = new Page(data, options);
  var measurer = new Measurer(data, options);
  var columns = new Columns(data, source.columns, options);

  var table = null;
  var tableDiv = null;
  var header = null;
  var footer = null;
  var tbody = null;

  // Caches pagedTable.clientWidth, specially for webkit
  var cachedPagedTableClientWidth = null;

  var columnNavigationWidthPX = 5;

  var renderColumnNavigation = function(direction) {
    var arrow = document.createElement("div");
    arrow.setAttribute("style",
      "border-top: " + columnNavigationWidthPX + "px solid transparent;" +
      "border-bottom: " + columnNavigationWidthPX + "px solid transparent;" +
      "border-" + (direction != "right" ? "right" : "left") + ": " + columnNavigationWidthPX + "px solid;");

    var header = document.createElement("th");

    header.appendChild(arrow);
    header.setAttribute("style",
      "cursor: pointer;" +
      "vertical-align: middle;" +
      "min-width: " + columnNavigationWidthPX + "px;" +
      "width: " + columnNavigationWidthPX + "px;");
    header.setAttribute("class",(direction != "right" ? "right" : "left") + "-arrow " +
                  (direction != "right" ? "right" : "left") + "-navigator-column");

    header.onclick = function() {
      columns.incColumnNumber(direction != "right");
      me.animateColumns(direction != "right");
      renderFooter();
      graduate_new_columns(columns.visCols, direction != "right");
    };

    header.on

    return header;
  };

  var maxColumnWidth = function(width) {
    var padding = 80;
    var columnMax = Math.max(cachedPagedTableClientWidth - padding, 0);

    return parseInt(width) > 0 ?
      Math.min(columnMax, parseInt(width)) + "px" :
      columnMax + "px";
  };

  var makeColumnHeaderElement = function(idx){
    var columnData = columns.subset[idx]

    var column = document.createElement("th");
    column.setAttribute("class", "col_" + idx)

    if (typeof(columnData.align) === "undefined") columnData.align = "left";
    column.setAttribute("align", columnData.align);
    column.style.textAlign = columnData.align;
    column.style.display = "";

    column.style.maxWidth = maxColumnWidth(null);
    if (columnData.width) {
      column.style.minWidth = column.style.maxWidth = maxColumnWidth(columnData.width);
    }

    var columnName = document.createElement("div");
    columnName.setAttribute("class", "pagedtable-header-name");
    if (columnData.label === "") {
      columnName.innerHTML = "&nbsp;";
    } else {
      columnName.appendChild(document.createTextNode(columnData.label));
    }
    column.appendChild(columnName);

    var columnType = document.createElement("div");
    columnType.setAttribute("class", "pagedtable-header-type");
    if (columnData.type === "") {
      columnType.innerHTML = "&nbsp;";
    } else {
      if (typeof(columnData.type) !== "undefined") {
        columnType.appendChild(document.createTextNode("<" + columnData.type + ">"));
      }
    }
    column.appendChild(columnType)
    return(column);
  };

  var makeColumnBodyElements = function(idx){

    var columnData = columns.subset[idx]

    var fragment = document.createDocumentFragment();

    // assumption that all fields have the same keys? valid?
    var idx_name = columnData.name

    var column_elements = []

    data.forEach(function(dataRow,idxRow) {

      var dataCell = dataRow[idx_name];
      var htmlCell = document.createElement("td");
      htmlCell.setAttribute("class", "col_" + idx)
      htmlCell.style.display = "&nbsp";




      if (typeof(dataCell) !== "string") dataCell = dataCell.toString();
      if (dataCell === "NA") htmlCell.setAttribute("class", "pagedtable-na-cell");
      if (dataCell === "__NA__") dataCell = "NA";

      var cellText = document.createElement('div');
      cellText.style.width = "fit-content"

      if(columns.widths[idx_name].html){
        cellText.innerHTML = makeCellContents(dataCell)
      }else{
        var cellText = document.createTextNode(dataCell);
      }

      htmlCell.appendChild(cellText);
      if (dataCell.length > 50 & !columns.widths[idx_name]["html"]) {
        htmlCell.setAttribute("title", dataCell);
      }
      htmlCell.setAttribute("align", columnData.align);
      htmlCell.style.textAlign = columnData.align;
      htmlCell.style.maxWidth = maxColumnWidth(null);
      if (columnData.width) {
        htmlCell.style.minWidth = htmlCell.style.maxWidth = maxColumnWidth(columnData.width);
      }

      column_elements[idxRow] = htmlCell;
    });


    //extra empty cells to generate
    var extraRows = data.length % page.rows;

    if(extraRows != 0 & page.total != 1){

      var extraRows = page.rows - extraRows;

      var idxRow = column_elements.length;
      var current_row = column_elements.length;

      while(idxRow < (current_row + extraRows)){

        var htmlCell = document.createElement("td");
        htmlCell.setAttribute("class", "col_" + idx)
        htmlCell.style.display = "";

        var cellText = document.createElement('div');
        cellText.style.width = "fit-content"

        cellText.innerHTML = "&nbsp;";
        htmlCell.appendChild(cellText);

        htmlCell.setAttribute("align", columnData.align);
        htmlCell.style.textAlign = columnData.align;
        htmlCell.style.maxWidth = maxColumnWidth(null);
        if (columnData.width) {
          htmlCell.style.minWidth = htmlCell.style.maxWidth = maxColumnWidth(columnData.width);
        }
        column_elements[idxRow] = htmlCell;
        idxRow += 1;
        }
    }


    return(column_elements)
  };

  me.toggleColumnNavigation = function(direction){

    var el_class = (direction == "right" ? "right" : "left") + "-navigator-column";
    me.toggleColumn(el_class);

  }

  me.toggleColumn = function(el_class){
    var colNav = pagedTable.querySelectorAll("." + el_class);

    colNav.forEach(function(el){
      el.style.display = el.style.display == "none" ? "" : "none"
    })
  }

  me.toggleRow = function(rownum){
    var row = pagedTable.querySelector(".row_" + rownum);
    row.style.visibility = row.style.visibility == "visible" ? "collapse" : "visible"
  }

  me.styleColumn = function(el_class, styles){
    var coltostyle = pagedTable.querySelectorAll("." + el_class);
    for(var s in styles) {
        coltostyle.forEach(function(e){
          e.style[s] = styles[s];
        });
    }
  };

 // to try to prevent double calculation, the columns must be created.
  me.makeColumn = function(column_idx, backwards){
    // add column to header
    var pt_header = pagedTable.querySelector("thead");
    var pt_body = pagedTable.querySelector("tbody");

    // If the column already exists, toggle its visibility, otherwise create it.
    var colExists = pt_header.querySelector(".col_" + column_idx) != null;
    if(colExists){
      me.toggleColumn("col_" + column_idx);
    }else{

      var header_element = makeColumnHeaderElement(column_idx);
      var body_elements = makeColumnBodyElements(column_idx);

      if(backwards){
        pt_header.querySelector(".right-navigator-column").after(header_element);
      } else{
        pt_header.querySelector(".left-navigator-column").before(header_element);
      }

      body_elements.map(function(el, idxRow){
        var row = pt_body.querySelector(".row_"+idxRow)

        if(backwards){
          row.querySelector(".right-navigator-column").after(el);
        } else{
          row.querySelector(".left-navigator-column").before(el);
        }
      })

      me.styleColumn("col_" + column_idx,{
        opacity:0,
        transition: "",
      })
    }

  };

  var forEachDir = function(array, direction, func){

    var arr = array;

    if(direction){
      arr = arr.slice().reverse();
    }

    arr.forEach(func);
  }

  var graduate_new_columns = function(cols, direction, delay){

    delay = typeof delay !== 'undefined' ? delay : 100;

    forEachDir(cols, direction, function(col_idx){
      setTimeout(function(){
        me.styleColumn("col_" + col_idx,{
          opacity : "1",
          transition : "opacity 200ms"
        })}, delay);
    })
  }

  me.animateColumns = function(backwards) {

    var currentCols = columns.visCols

    var startCol = backwards ? currentCols[0] -1 : currentCols[currentCols.length - 1] + 1 ;

    me.addTableContents(startCol, backwards);

    var newCols = columns.visCols

    // disappear the old columns
    forEachDir(currentCols, backwards, function(col_idx){
      me.toggleColumn("col_" + col_idx);
      me.styleColumn("col_" + col_idx,{
        opacity: 0,
        transition: "",
      })
    })

    // rules about showing the left/right arrows
    if(newCols.includes(columns.total-1)){
      me.toggleColumnNavigation("left");
    }

    if(currentCols[0] === 0 & !newCols.includes(0)){
      me.toggleColumnNavigation("right")
    }

    graduate_new_columns(newCols, backwards);

    if(currentCols[currentCols.length - 1] === columns.total-1 & backwards){
      me.toggleColumnNavigation("left")
    }

    if(newCols.includes(0) & !currentCols.includes(0)){
      me.toggleColumnNavigation("right");
    }

    renderFooter();

  }

  me.updateDisplayedPage = function(oldRows, newRows){
    oldRows.forEach(function(x){
      me.toggleRow(x);
    })

    newRows.forEach(function(x){
      me.toggleRow(x);
    })

  }

  me.onChange = function(callback) {
    onChangeCallbacks.push(callback);
  };

  var triggerOnChange = function() {
    onChangeCallbacks.forEach(function(onChange) {
      onChange();
    });
  };

  var getLabelInfo = function() {
    var pageStart = page.getRowStart();
    var pageEnd = page.getVisRowEnd();
    var totalRows = data.length;

    var totalRowsLabel = options.rows.total ? options.rows.total : totalRows;
    var totalRowsLabelFormat = totalRowsLabel.toString().replace(/(\d)(?=(\d\d\d)+(?!\d))/g, '$1,');

    var infoText = (pageStart + 1) + "-" + pageEnd + " of " + totalRowsLabelFormat + " rows";
    if (totalRows < page.rows) {
      infoText = totalRowsLabel + " row" + (totalRows != 1 ? "s" : "");
    }
    if (columns.total > columns.visCols.length) {
      var totalColumnsLabel = options.columns.total ? options.columns.total : columns.total;

      infoText = infoText + " | " + (columns.visCols[0] + 1) + "-" +
        (columns.visCols[columns.visCols.length - 1] + 1)  +
        " of " + totalColumnsLabel + " columns";
    }

    return infoText;
  };

  var clearFooter = function() {
    footer = pagedTable.querySelectorAll("div.pagedtable-footer")[0];
    footer.innerHTML = "";

    return footer;
  };

  var createPageLink = function(idxPage) {
    var pageLink = document.createElement("a");
    pageLinkClass = idxPage === page.number ? "pagedtable-index pagedtable-index-current" : "pagedtable-index";
    pageLink.setAttribute("class", pageLinkClass);
    pageLink.setAttribute("data-page-index", idxPage);
    pageLink.onclick = function() {
      var oldRows = page.getVisRows()
      page.setPageNumber(parseInt(this.getAttribute("data-page-index")));
      var newRows = page.getVisRows()
      me.updateDisplayedPage(oldRows, newRows);

      renderFooter();
    };

    pageLink.appendChild(document.createTextNode(idxPage + 1));

    return pageLink;
  }

  var renderFooter = function() {
    footer = clearFooter();

    var next = document.createElement("a");
    next.appendChild(document.createTextNode("Next"));

    next.onclick = function() {
      var oldRows = page.getVisRows()
      page.setPageNumber(page.number + 1);
      var newRows = page.getVisRows()
      me.updateDisplayedPage(oldRows, newRows);
      renderFooter();
    };

    if (data.length > page.rows) footer.appendChild(next);

    var pageNumbers = document.createElement("div");
    pageNumbers.setAttribute("class", "pagedtable-indexes");

    var pageRange = page.getVisiblePageRange();

    if (pageRange.first) {
      var pageLink = createPageLink(0);
      pageNumbers.appendChild(pageLink);

      var pageSeparator = document.createElement("div");
      pageSeparator.setAttribute("class", "pagedtable-index-separator-left");
      pageSeparator.appendChild(document.createTextNode("..."))
      pageNumbers.appendChild(pageSeparator);
    }

    for (var idxPage = pageRange.start; idxPage < pageRange.end; idxPage++) {
      var pageLink = createPageLink(idxPage);

      pageNumbers.appendChild(pageLink);
    }

    if (pageRange.last) {
      var pageSeparator = document.createElement("div");
      pageSeparator.setAttribute("class", "pagedtable-index-separator-right");
      pageSeparator.appendChild(document.createTextNode("..."))
      pageNumbers.appendChild(pageSeparator);

      var pageLink = createPageLink(page.total - 1);
      pageNumbers.appendChild(pageLink);
    }

    if (data.length > page.rows) footer.appendChild(pageNumbers);

    var previous = document.createElement("a");
    previous.appendChild(document.createTextNode("Previous"));
    previous.onclick = function() {
      var oldRows = page.getVisRows()
      page.setPageNumber(page.number - 1);
      var newRows = page.getVisRows()
      me.updateDisplayedPage(oldRows, newRows);
      renderFooter();
    };
    if (data.length > page.rows) footer.appendChild(previous);

    var infoLabel = document.createElement("div");
    infoLabel.setAttribute("class", "pagedtable-info");
    infoLabel.setAttribute("title", getLabelInfo());
    infoLabel.appendChild(document.createTextNode(getLabelInfo()));
    footer.appendChild(infoLabel);

    var enabledClass = "pagedtable-index-nav";
    var disabledClass = "pagedtable-index-nav pagedtable-index-nav-disabled";
    previous.setAttribute("class", page.number <= 0 ? disabledClass : enabledClass);
    next.setAttribute("class", (page.number + 1) * page.rows >= data.length ? disabledClass : enabledClass);
  };

  var measuresCell = null;

  var renderMeasures = function() {
    var measuresTable = document.createElement("table");
    measuresTable.style.visibility = "hidden";
    measuresTable.style.position = "absolute";
    measuresTable.style.whiteSpace = "nowrap";
    measuresTable.style.height = "auto";
    measuresTable.style.width = "auto";

    var measuresRow = document.createElement("tr");
    measuresTable.appendChild(measuresRow);

    measuresCell = document.createElement("td");
    var sampleString = "ABCDEFGHIJ0123456789";
    measuresCell.appendChild(document.createTextNode(sampleString));

    measuresRow.appendChild(measuresCell);

    tableDiv.appendChild(measuresTable);
  }

  // Create paged table shell
  var drawTableShell = function() {

    tableDiv = document.createElement("div");
    pagedTable.appendChild(tableDiv);
    var pagedTableClass = data.length > 0 ?
      "pagedtable pagedtable-not-empty" :
      "pagedtable pagedtable-empty";

    if (columns.total == 0 || (columns.emptyNames() && data.length == 0)) {
      pagedTableClass = pagedTableClass + " pagedtable-empty-columns";
    }

    tableDiv.setAttribute("class", pagedTableClass);

    table = document.createElement("table");
    table.setAttribute("cellspacing", "0");
    table.setAttribute("class", "table table-condensed");
    tableDiv.appendChild(table);

    table.appendChild(document.createElement("thead"));

    var footerDiv = document.createElement("div");
    footerDiv.setAttribute("class", "pagedtable-footer");
    tableDiv.appendChild(footerDiv);

    // if the host has not yet provided horizontal space, render hidden
    if (tableDiv.clientWidth <= 0) {
      tableDiv.style.opacity = "0";
    }

  }

  // create table shell initial contents
  var drawTableShellContents = function(){

    cachedPagedTableClientWidth = pagedTable.clientWidth;
    var table = pagedTable.querySelector("table");

    var fragment = document.createDocumentFragment();
    //Header
    var header = table.querySelector("thead");
    var tableheader_row = document.createElement("tr");
    header.appendChild(tableheader_row);

    tableheader_row.appendChild(renderColumnNavigation("left"));
    tableheader_row.appendChild(renderColumnNavigation("right"));

    var visRowsPage = page.getVisRows();

    //Body
    data.forEach(function(dataRow, idxRow) {

      var htmlRow = document.createElement("tr");
      htmlRow.setAttribute("class", ((idxRow % 2 !==0) ? "even" : "odd") + " row_"+idxRow);
      if(visRowsPage.includes(idxRow)){
          htmlRow.style.visibility = "visible";
      }else{
          htmlRow.style.visibility = "collapse";
      }

      var left_nav_cell = document.createElement("td");
      left_nav_cell.setAttribute("class","right-navigator-column");
      var right_nav_cell = document.createElement("td");
      right_nav_cell.setAttribute("class","left-navigator-column");

      htmlRow.appendChild(left_nav_cell);
      htmlRow.appendChild(right_nav_cell)

      fragment.appendChild(htmlRow);
    });

     //extra empty cells to generate
    var extraRows = data.length % page.rows;

    if(extraRows != 0 & page.total != 1){

      var extraRows = page.rows - extraRows;

      var idx = data.length;
      var current_row = data.length;

      while(idx < (current_row + extraRows)){

        var htmlRow = document.createElement("tr");
        htmlRow.setAttribute("class", "row_"+ idx);
        if(visRowsPage.includes(idx)){
            htmlRow.style.visibility = "visible";
        }else{
            htmlRow.style.visibility = "collapse";
        }

        var left_nav_cell = document.createElement("td");
        left_nav_cell.setAttribute("class","right-navigator-column");
        var right_nav_cell = document.createElement("td");
        right_nav_cell.setAttribute("class","left-navigator-column");

        htmlRow.appendChild(left_nav_cell);
        htmlRow.appendChild(right_nav_cell)

        fragment.appendChild(htmlRow);
        idx += 1;
        }
    }




    tbody = document.createElement("tbody");
    tbody.appendChild(fragment);

    table.appendChild(tbody);

  }

  // draw table framework/shell to be filled
  me.drawTable = function(){
    drawTableShell();
    drawTableShellContents();
  };

  me.addTableContents = function(startCol, backwards){

    if (tableDiv.clientWidth > 0) {
      tableDiv.style.opacity = 1;
    }

    var visibleColumns = []
    var columnNumber = startCol;

    var tableDivStyle = window.getComputedStyle(tableDiv, null);
    var tableDivPadding = parsePadding(tableDivStyle.paddingLeft) +
      parsePadding(tableDivStyle.paddingRight);

    var currentWidth = 0;

    while (true) {

      var columnWidth = columns.getColWidth(columnNumber)
      var currentWidth = currentWidth + columnWidth[0];

      //If we know the width of the element, and it will be too wide, nah dog, break out of this loop

      if (tableDiv.clientWidth - tableDivPadding < currentWidth) {
        break;
      }

      me.makeColumn(columnNumber, backwards)

      // we do not pre-define html element widths, so who knows how wide it will be?
      if(columnWidth[1]){

        var html_column = pagedTable.querySelector("table").querySelector("tbody").querySelectorAll(".col_" + columnNumber);
        // get width for each element and compare against current persribed width
        html_column.forEach(function(el){
          columnWidth[0] = Math.max( columnWidth[0],
          el.children[0].offsetWidth);
        })
        currentWidth = currentWidth + columnWidth[0]
        columns.setColWidth(columnNumber, columnWidth[0]);

        if (tableDiv.clientWidth - tableDivPadding < currentWidth) {
          // we need to hide the column that is too wide
          // hiding prevents having to re-generate
          me.toggleColumn("col_" + columnNumber);
          break;
        }

      }

      visibleColumns.push(columnNumber)

      // dont try to add more fields columns than exist
      if( columnNumber  === 0 ) {
        // if we are moving backwards and hit the first column, but did
        // not fill the table, repopulate with the "old" columns
        if( tableDiv.clientWidth - tableDivPadding > currentWidth){
          if( backwards ){
            backwards = !backwards
            columnNumber = visibleColumns[0]
            visibleColumns = visibleColumns.slice().reverse();
          }
        } else if( backwards ){
          break
        };
      }

      if( columnNumber === columns.total-1 ) {
        // if we are moving to the right and hit the last column, but did
        // not fill the table, repopulate with the "old" columns
        if( tableDiv.clientWidth - tableDivPadding > currentWidth ){
          if( !backwards & visibleColumns[0] != 0){
            backwards = !backwards
            columnNumber = visibleColumns[0]
            visibleColumns = visibleColumns.slice().reverse();
          } else if( !backwards & visibleColumns[0] == 0 ){
            break
          }
        } else if( !backwards ){
          break
        };
      }


      if(backwards){
        columnNumber -= 1;
      } else {
        columnNumber += 1;
      }
    }

    if(backwards){
      visibleColumns = visibleColumns.slice().reverse();
    }

    columns.setVisibleColumns(visibleColumns)

  };

  // called on initialization of the table, and should be used to first draw table
  me.init = function() {

    //create framework table
    me.drawTable()

    //initialization of stuff
    renderMeasures();
    measurer.calculate(measuresCell);
    columns.calculateWidths(measurer.measures);

    //add table contents
    me.addTableContents(0, false);

    renderFooter();

    me.toggleColumnNavigation("right");

    if(columns.visCols.length === columns.total){
      me.toggleColumnNavigation("left");
    }

    graduate_new_columns(columns.visCols, false);


    // retry seizing columns later if the host has not provided space
    function retryFit() {
      if (tableDiv.clientWidth <= 0) {
        setTimeout(retryFit, 100);
      } else {
        triggerOnChange();
      }
    }
    if (tableDiv.clientWidth <= 0) {
      retryFit();
    }

    return me;
  };

  var registerWidths = function() {
    columns.subset = columns.subset.map(function(column) {
      column.width = columns.widths[column.name].inner;
      return column;
    });
  };

  me.updateView = function() {
    renderMeasures();
    measurer.calculate(measuresCell);
    columns.calculateWidths(measurer.measures);

    var currentCols = columns.visCols

    //hide current nav bars
    if(currentCols[0] != 0){
      me.toggleColumnNavigation("right", true);
    }
    if(currentCols[currentCols.length -1 ] != (columns.total -1)){
      me.toggleColumnNavigation("left", true);
    }


    // hide current columns
    forEachDir(currentCols, false, function(col_idx){
      me.toggleColumn("col_" + col_idx);
    })

    // add table contents
    me.addTableContents(currentCols[0], false);

    var newCols  = columns.visCols

    // toggle "right" navigator
    if(currentCols[currentCols.length -1] == (columns.total-1)){
      if(newCols[newCols.length -1] != (columns.total-1)){
        me.toggleColumnNavigation("left", true);
      }
    }else{
      if(newCols[newCols.length -1] == (columns.total-1)){
        me.toggleColumnNavigation("left", true);
      }
    }

    // toggle "left" navigator
    if(currentCols[0] == 0){
      if(newCols[0] != 0){
        me.toggleColumnNavigation("right", true);
      }
    }else{
      if(newCols[0] == 0){
        me.toggleColumnNavigation("right", true);
      }
    }

    //If there are new columns, transition them in

    let difference = newCols.filter(x => !currentCols.includes(x));

    forEachDir(difference, false, function(col_idx){
      me.styleColumn("col_" + col_idx,{
        opacity:0,
        transition: ""
      });
    })

    graduate_new_columns(difference, false, 100)


    // "redisplay" current nav bars
    if(currentCols[0] != 0){
      me.toggleColumnNavigation("right", true);
    }
    if(currentCols[currentCols.length -1 ] != (columns.total -1)){
      me.toggleColumnNavigation("left", true);
    }

    renderFooter();
  }

  var parsePadding = function(value) {
    return parseInt(value) >= 0 ? parseInt(value) : 0;
  };

  me.fixedHeight = function() {
    return options.rows.max != null;
  }

  me.fitRows = function() {
    if (me.fixedHeight())
      return;

    measurer.calculate(measuresCell);

    var rows = options.rows.min !== null ? options.rows.min : 0;
    var headerHeight = header !== null && header.offsetHeight > 0 ? header.offsetHeight : 0;
    var footerHeight = footer !== null && footer.offsetHeight > 0 ? footer.offsetHeight : 0;

    if (pagedTable.offsetHeight > 0) {
      var availableHeight = pagedTable.offsetHeight - headerHeight - footerHeight;
      rows = Math.floor((availableHeight) / measurer.measures.height);
    }

    rows = options.rows.min !== null ? Math.max(options.rows.min, rows) : rows;

    page.setRows(rows);
  }

  var resizeLastWidth = -1;
  var resizeLastHeight = -1;
  var resizeNewWidth = -1;
  var resizeNewHeight = -1;
  var resizePending = false;

  me.resize = function(newWidth, newHeight) {

    function resizeDelayed() {
      resizePending = false;

      if (
        (resizeNewWidth !== resizeLastWidth) ||
        (!me.fixedHeight() && resizeNewHeight !== resizeLastHeight)
      ) {
        resizeLastWidth = resizeNewWidth;
        resizeLastHeight = resizeNewHeight;

        setTimeout(resizeDelayed, 200);
        resizePending = true;
      } else {
        triggerOnChange();

        resizeLastWidth = -1;
        resizeLastHeight = -1;
      }
    }

    resizeNewWidth = newWidth;
    resizeNewHeight = newHeight;

    if (!resizePending) resizeDelayed();
  };
};

var PagedTableDoc;
(function (PagedTableDoc) {
  var allPagedTables = [];

  PagedTableDoc.initAll = function() {
    allPagedTables = [];

    var pagedTables = [].slice.call(document.querySelectorAll('[data-pagedtable="false"],[data-pagedtable=""]'));
    pagedTables.forEach(function(pagedTable, idx) {
      pagedTable.setAttribute("data-pagedtable", "true");
      pagedTable.setAttribute("pagedtable-page", 0);
      pagedTable.setAttribute("class", "pagedtable-wrapper");

      if (pagedTable.hasAttribute("data-pagedtable-source")) {
        var xmlhttp = new XMLHttpRequest();
        var url = pagedTable.getAttribute("data-pagedtable-source");

        xmlhttp.onreadystatechange = function() {
          if (this.readyState == 4 && this.status == 200) {
            var source = JSON.parse(this.responseText);

            allPagedTables.push((new PagedTable(pagedTable, source)).init());
          }
        };
        xmlhttp.open("GET", url, true);
        xmlhttp.send();
      }
      else {
        var pagedTableInstance = new PagedTable(pagedTable);
        pagedTableInstance.init();

        allPagedTables.push(pagedTableInstance);
      }
    });
  };

  PagedTableDoc.resizeAll = function() {
    allPagedTables.forEach(function(pagedTable) {
      pagedTable.updateView();
    });
  };

  window.addEventListener("resize", PagedTableDoc.resizeAll);

  return PagedTableDoc;
})(PagedTableDoc || (PagedTableDoc = {}));

window.onload = function() {
  PagedTableDoc.initAll();
};

pagedtable = {
  create: function(dataframe, element, options) {
    if (typeof(options) === "undefined") options = {};
    (new PagedTable(element, Object.assign(dataframe, { options: options }))).init();
    return dataframe;
  }
};
