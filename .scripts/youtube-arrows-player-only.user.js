// ==UserScript==
// @name         YouTube arrows → player only
// @namespace    https://violentmonkey.github.io/
// @version      1.0
// @description  Left/Right always seek, Up/Down always change volume of the open player, whatever has focus. Arrows never scroll; with no player they do nothing.
// @match        https://www.youtube.com/*
// @run-at       document-start
// @grant        none
// ==/UserScript==

(() => {
    'use strict';

    const SEEK_STEP = 5;               // seconds per Left/Right
    const VOLUME_STEP = 5;             // percent per Up/Down
    const ALLOW_IN_TEXT_FIELDS = true; // false = arrows hit the player even while typing in search/comments

    const ACTIONS = {
        ArrowLeft:  p => seek(p, -SEEK_STEP),
        ArrowRight: p => seek(p, +SEEK_STEP),
        ArrowUp:    p => volume(p, +VOLUME_STEP),
        ArrowDown:  p => volume(p, -VOLUME_STEP),
    };

    // Inside same-origin frames (e.g. live chat), drive the top page's player.
    const doc = (() => { try { return window.top.document; } catch { return document; } })();

    function isTextField(el) {
        if (!el || el.nodeType !== 1) return false;
        if (el.isContentEditable || el.tagName === 'TEXTAREA') return true;
        if (el.tagName === 'INPUT') {
            return !/^(button|checkbox|radio|range|submit|reset|color|file|image)$/.test(el.type);
        }
        return false;
    }

    function isShown(el) {
        if (!el || !el.isConnected) return false;
        if (el.checkVisibility && !el.checkVisibility()) return false;
        const r = el.getBoundingClientRect();
        return r.width > 0 && r.height > 0;
    }

    // Watch page / miniplayer = #movie_player, Shorts = #shorts-player.
    // A watch player left hidden after navigating away is ignored (not shown).
    function findPlayer() {
        const ids = doc.location.pathname.startsWith('/shorts')
            ? ['shorts-player', 'movie_player']
            : ['movie_player', 'shorts-player'];
        for (const id of ids) {
            const p = doc.getElementById(id);
            if (isShown(p)) return p;
        }
        // Any other real player (e.g. channel trailer), never hover previews.
        for (const p of doc.querySelectorAll('.html5-video-player')) {
            if (!p.closest('ytd-video-preview, #video-preview, #inline-preview-player') && isShown(p)) return p;
        }
        return null;
    }

    const fmt = s => {
        s = Math.max(0, Math.floor(s));
        const h = Math.floor(s / 3600), m = Math.floor(s / 60) % 60, sec = String(s % 60).padStart(2, '0');
        return h ? `${h}:${String(m).padStart(2, '0')}:${sec}` : `${m}:${sec}`;
    };

    // Prefer YouTube's player API (keeps UI + saved volume in sync); fall back to the <video>.
    function seek(p, delta) {
        const api = typeof p.seekTo === 'function' && typeof p.getCurrentTime === 'function';
        const v = p.querySelector('video');
        if (!api && !v) return null;
        const now = api ? p.getCurrentTime() : v.currentTime;
        const dur = (api ? p.getDuration?.() : v.duration) || Infinity;
        const t = Math.min(Math.max(now + delta, 0), dur);
        if (api) p.seekTo(t, true); else v.currentTime = t;
        return `${delta > 0 ? '+' : '−'}${Math.abs(delta)}s · ${fmt(t)}`;
    }

    function volume(p, delta) {
        if (typeof p.getVolume === 'function' && typeof p.setVolume === 'function') {
            if (delta > 0 && p.isMuted?.()) p.unMute();
            const vol = Math.round(Math.min(100, Math.max(0, p.getVolume() + delta)));
            p.setVolume(vol);
            return p.isMuted?.() ? 'Muted' : `Volume ${vol}%`;
        }
        const v = p.querySelector('video');
        if (!v) return null;
        if (delta > 0) v.muted = false;
        v.volume = Math.min(1, Math.max(0, v.volume + delta / 100));
        return v.muted ? 'Muted' : `Volume ${Math.round(v.volume * 100)}%`;
    }

    // Small on-screen indicator inside the player (works in fullscreen too).
    let osd, osdTimer;
    function flash(p, text) {
        if (!text) return;
        if (!osd) {
            osd = doc.createElement('div');
            Object.assign(osd.style, {
                position: 'absolute', top: '10%', left: '50%', transform: 'translateX(-50%)',
                padding: '6px 14px', borderRadius: '6px', background: 'rgba(0,0,0,.7)', color: '#fff',
                font: '500 16px/1.4 Roboto, Arial, sans-serif', zIndex: '9999', pointerEvents: 'none',
                transition: 'opacity .3s', opacity: '0',
            });
        }
        if (osd.parentNode !== p) p.appendChild(osd);
        osd.textContent = text;
        osd.style.opacity = '1';
        clearTimeout(osdTimer);
        osdTimer = setTimeout(() => { osd.style.opacity = '0'; }, 700);
    }

    function onKey(e) {
        const action = ACTIONS[e.key];
        if (!action) return;
        if (e.ctrlKey || e.altKey || e.metaKey || e.shiftKey) return; // leave Alt+←/→ (back/forward) etc. alone
        if (ALLOW_IN_TEXT_FIELDS && isTextField(e.composedPath()[0])) return;

        // Registered first (document-start, window, capture): nothing else on the page sees the arrow.
        // No scrolling, no slider/menu focus behaviour, no Shorts up/down navigation.
        e.preventDefault();
        e.stopImmediatePropagation();
        if (e.type !== 'keydown') return;

        const p = findPlayer();
        if (p) flash(p, action(p));
    }

    window.addEventListener('keydown', onKey, true);
    window.addEventListener('keyup', onKey, true);
})();
