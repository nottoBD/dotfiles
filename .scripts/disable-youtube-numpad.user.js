// ==UserScript==
// @name         Disable YouTube Numpad
// @namespace    https://violentmonkey.github.io/
// @version      1.3
// @description  Disable numpad 0-9, Home, and End keys on YouTube video pages (top-row digits still work)
// @author       Alyssa B. Morton
// @license      MIT
// @match        https://www.youtube.com/*
// @grant        none
// ==/UserScript==

(function() {
    'use strict';

    // Keys to block (top-row Digit0-9 intentionally left out)
    const blockedKeys = [
        'Numpad0','Numpad1','Numpad2','Numpad3','Numpad4','Numpad5','Numpad6','Numpad7','Numpad8','Numpad9', // Numpad
        'Home','End' // Home/End keys
    ];

    // Only apply on video pages
    function isVideoPage() {
        return window.location.pathname === '/watch';
    }

    document.addEventListener('keydown', function(e) {
        if (!isVideoPage()) return; // Do nothing if not a video page

        // Only block if no modifier keys
        if (!e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey) {
            if (blockedKeys.includes(e.code)) {
                e.stopPropagation();
                e.preventDefault();
            }
        }
    }, true); // capture phase to override YouTube listeners

})();
