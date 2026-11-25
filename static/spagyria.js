/* Prontuario delle tinture spagiriche — legge /static/plants.json */
(function () {
  "use strict";

  var root = document.getElementById("spg");
  if (!root) return;

  var I18N = {
    it: {
      disclaimer: "Prontuario delle tinture spagiriche che ho preparato personalmente. Le informazioni hanno carattere divulgativo e tradizionale e non sostituiscono in alcun modo il parere di un medico.",
      tabPlants: "Piante",
      tabIndications: "Per disturbo",
      searchPlaceholder: "Cerca una pianta, un disturbo, una proprietà…",
      clear: "Cancella ricerca",
      sort: "Ordina",
      sortAz: "A → Z",
      sortSeq: "N° preparazione",
      expandAll: "Espandi tutto",
      collapseAll: "Chiudi tutto",
      index: "Indice delle piante",
      noResults: "Nessun risultato. Prova con un'altra parola o rimuovi i filtri.",
      backToTop: "Torna su",
      plants: function (n) { return n === 1 ? "1 pianta" : n + " piante"; },
      indications: function (n) { return n === 1 ? "1 disturbo" : n + " disturbi"; },
      foundIn: "Trovato in",
      planets: "Pianeti",
      signs: "Segni",
      allPlanets: "Tutti",
      sections: {
        avvertenze: "Avvertenze",
        indicazioni: "Indicazioni",
        descrizione: "Proprietà e descrizione",
        psicosomatica: "Effetto psicosomatico",
        uso_esterno: "Uso esterno",
        parti_utilizzate: "Parti utilizzate",
        osservazioni: "Osservazioni",
        note_astrologiche: "Note astrologiche",
        note: "Note personali",
        preparazioni: "Preparazioni",
        fonti: "Fonti",
        nome: "Nome"
      },
      pending: "Scheda in preparazione.",
      seq: "N°",
      copyLink: "Copia link alla scheda",
      copied: "Link copiato"
    },
    en: {
      disclaimer: "A handbook of the spagyric tinctures I have personally prepared (content in Italian). The information is traditional and for general interest only, and is no substitute for medical advice.",
      tabPlants: "Plants",
      tabIndications: "By ailment",
      searchPlaceholder: "Search a plant, an ailment, a property…",
      clear: "Clear search",
      sort: "Sort",
      sortAz: "A → Z",
      sortSeq: "Preparation no.",
      expandAll: "Expand all",
      collapseAll: "Collapse all",
      index: "Plant index",
      noResults: "No results. Try another word or remove the filters.",
      backToTop: "Back to top",
      plants: function (n) { return n === 1 ? "1 plant" : n + " plants"; },
      indications: function (n) { return n === 1 ? "1 ailment" : n + " ailments"; },
      foundIn: "Found in",
      planets: "Planets",
      signs: "Signs",
      allPlanets: "All",
      sections: {
        avvertenze: "Warnings",
        indicazioni: "Indications",
        descrizione: "Properties and description",
        psicosomatica: "Psychosomatic effect",
        uso_esterno: "External use",
        parti_utilizzate: "Parts used",
        osservazioni: "Observations",
        note_astrologiche: "Astrological notes",
        note: "Personal notes",
        preparazioni: "Preparations",
        fonti: "Sources",
        nome: "Name"
      },
      pending: "Entry in progress.",
      seq: "No.",
      copyLink: "Copy link to this entry",
      copied: "Link copied"
    }
  };
  var T = I18N[root.getAttribute("data-lang")] || I18N.it;

  var PLANETS = [
    ["Sole", "☉"], ["Luna", "☽"], ["Mercurio", "☿"], ["Venere", "♀"],
    ["Marte", "♂"], ["Giove", "♃"], ["Saturno", "♄"]
  ];
  var PLANET_SYM = {};
  PLANETS.forEach(function (p) { PLANET_SYM[p[0]] = p[1] + "︎"; });
  var SIGN_SYM = {
    Ariete: "♈", Toro: "♉", Gemelli: "♊", Cancro: "♋", Leone: "♌", Vergine: "♍",
    Bilancia: "♎", Scorpione: "♏", Sagittario: "♐", Capricorno: "♑", Acquario: "♒", Pesci: "♓"
  };

  // Sections shown in the body of a card, in reading order.
  var TEXT_SECTIONS = ["descrizione", "psicosomatica", "uso_esterno", "parti_utilizzate",
                       "osservazioni", "note_astrologiche", "note"];
  var SEARCH_FIELDS = ["nome", "nomeef", "indicazioni", "avvertenze"].concat(TEXT_SECTIONS);

  // A preparation whose code starts with "F" is not published (e.g. "F7").
  function published(prep) { return !/^F/i.test(String(prep.seq)); }
  function seqNumber(p) { return p._preps.length ? parseInt(p._preps[0].seq, 10) : Infinity; }

  var $ = function (id) { return document.getElementById(id); };
  var state = { view: "piante", q: "", planet: null, sort: "az" };
  var plants = [];

  /* ---------- helpers ---------- */

  function esc(s) {
    return String(s).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c];
    });
  }

  function fold(s) { // lower case, no accents
    return String(s || "").toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "");
  }

  function slug(s) {
    return fold(s).replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
  }

  function prose(text) {
    return String(text).split(/\n{2,}/).map(function (p) {
      return "<p>" + esc(p.trim()).replace(/\n/g, "<br />") + "</p>";
    }).join("");
  }

  function linkify(s) {
    var e = esc(s);
    if (/^https?:\/\//.test(s)) {
      var label = s.replace(/^https?:\/\/(www\.)?/, "").replace(/\/.*$/, "");
      return '<a href="' + e + '" target="_blank" rel="noopener noreferrer">' + esc(label) + "</a>";
    }
    return e;
  }

  function terms() {
    return fold(state.q).split(/\s+/).filter(Boolean);
  }

  /* ---------- matching ---------- */

  function fieldText(p, f) {
    var v = p[f];
    return Array.isArray(v) ? v.join(" ") : (v || "");
  }

  // Returns null if not matching, otherwise the list of fields that matched.
  function match(p, ts) {
    if (state.planet && p.pianeti.indexOf(state.planet) < 0) return null;
    if (!ts.length) return [];
    var hay = p._hay;
    for (var i = 0; i < ts.length; i++) if (hay.indexOf(ts[i]) < 0) return null;
    return SEARCH_FIELDS.filter(function (f) {
      var t = p._fold[f];
      return t && ts.some(function (x) { return t.indexOf(x) >= 0; });
    });
  }

  /* ---------- rendering ---------- */

  function planetBadges(list) {
    return list.map(function (pl) {
      return '<span class="spg-badge spg-planet" title="' + esc(pl) + '">' +
        '<span aria-hidden="true">' + (PLANET_SYM[pl] || "") + "</span> " + esc(pl) + "</span>";
    }).join("");
  }

  function signBadges(list) {
    return list.map(function (s) {
      var sym = SIGN_SYM[s] ? '<span aria-hidden="true">' + SIGN_SYM[s] + "︎</span> " : "";
      return '<span class="spg-badge spg-sign">' + sym + esc(s) + "</span>";
    }).join("");
  }

  function card(p) {
    var S = T.sections, h = [];
    h.push('<details class="spg-card" id="' + p._id + '" data-id="' + p._id + '">');
    h.push('<summary><span class="spg-title">');
    h.push('<span class="spg-name">' + esc(p.nome) + "</span>");
    if (p.nomeef) h.push('<span class="spg-alt">' + esc(p.nomeef) + "</span>");
    h.push('</span><span class="spg-meta">');
    h.push(p.pianeti.map(function (pl) {
      return '<span class="spg-sym" title="' + esc(pl) + '">' + PLANET_SYM[pl] + "</span>";
    }).join(""));
    if (p.avvertenze) h.push('<span class="spg-warn-dot" title="' + esc(S.avvertenze) + '">!</span>');
    h.push('<span class="spg-seq">' + T.seq + " " + p._preps.map(function (x) { return esc(x.seq); }).join(" · ") + "</span>");
    h.push('</span><span class="spg-found"></span></summary>');

    h.push('<div class="spg-body">');
    h.push('<div class="spg-astro">');
    h.push('<div><span class="spg-label">' + S.preparazioni + "</span>" + p._preps.map(function (x) {
      var variant = x.nome && x.nome !== p.nome ? " · <em>" + esc(x.nome) + "</em>" : "";
      return '<span class="spg-badge">' + T.seq + " " + esc(x.seq) + variant + "</span>";
    }).join("") + "</div>");
    if (p.pianeti.length || p.segni.length) {
      if (p.pianeti.length) h.push('<div><span class="spg-label">' + T.planets + "</span>" + planetBadges(p.pianeti) + "</div>");
      if (p.segni.length) h.push('<div><span class="spg-label">' + T.signs + "</span>" + signBadges(p.segni) + "</div>");
    }
    h.push("</div>");
    if (p.avvertenze) {
      h.push('<div class="spg-warning" role="note"><strong>⚠︎ ' + S.avvertenze + "</strong>" + prose(p.avvertenze) + "</div>");
    }
    if (p.indicazioni.length) {
      h.push('<section><h4>' + S.indicazioni + '</h4><ul class="spg-tags">');
      p.indicazioni.forEach(function (i) {
        h.push('<li><button type="button" class="spg-tag" data-search="' + esc(i) + '">' + esc(i) + "</button></li>");
      });
      h.push("</ul></section>");
    }
    TEXT_SECTIONS.forEach(function (f) {
      if (p[f]) h.push('<section class="spg-sec-' + f + '"><h4>' + S[f] + "</h4>" + prose(p[f]) + "</section>");
    });
    if (!p.descrizione && !p.indicazioni.length) h.push('<p class="spg-pending">' + T.pending + "</p>");
    if (p.fonti && p.fonti.length) {
      h.push('<section class="spg-sources"><h4>' + S.fonti + "</h4><ul>" +
        p.fonti.map(function (f) { return "<li>" + linkify(f) + "</li>"; }).join("") + "</ul></section>");
    }
    h.push('<p class="spg-cardfoot"><a href="#' + p._id + '" class="spg-permalink" data-copy="' + p._id + '">🔗︎ ' + T.copyLink + "</a></p>");
    h.push("</div></details>");
    return h.join("");
  }

  function buildPlanetFilter() {
    var used = {};
    plants.forEach(function (p) { p.pianeti.forEach(function (pl) { used[pl] = true; }); });
    var h = ['<button type="button" class="spg-chip" data-planet="" aria-pressed="true">' + T.allPlanets + "</button>"];
    PLANETS.forEach(function (p) {
      if (!used[p[0]]) return;
      h.push('<button type="button" class="spg-chip" data-planet="' + p[0] + '" aria-pressed="false" title="' + p[0] + '">' +
        '<span aria-hidden="true">' + PLANET_SYM[p[0]] + "</span> " + p[0] + "</button>");
    });
    $("spg-planets").innerHTML = h.join("");
  }

  function sorted() {
    var list = plants.slice();
    if (state.sort === "seq") {
      list.sort(function (a, b) { return (seqNumber(a) - seqNumber(b)) || a.nome.localeCompare(b.nome, "it"); });
    } else {
      list.sort(function (a, b) { return a.nome.localeCompare(b.nome, "it"); });
    }
    return list;
  }

  function renderCards() {
    $("spg-cards").innerHTML = sorted().map(card).join("");
  }

  function applyPlantsFilter() {
    var ts = terms(), shown = 0, index = [];
    sorted().forEach(function (p) {
      var el = $(p._id);
      var m = match(p, ts);
      el.hidden = m === null;
      var found = el.querySelector(".spg-found");
      if (m && m.length && ts.length) {
        found.textContent = T.foundIn + ": " + m.map(function (f) { return T.sections[f] || f; }).join(", ");
      } else {
        found.textContent = "";
      }
      if (m !== null) {
        shown++;
        index.push('<a href="#' + p._id + '" data-goto="' + p._id + '">' + esc(p.nome) + "</a>");
      }
    });
    $("spg-index").innerHTML = index.join("");
    // With very few results, open them straight away.
    if (ts.length && shown > 0 && shown <= 3) {
      root.querySelectorAll(".spg-card:not([hidden])").forEach(function (d) { d.open = true; });
    }
    $("spg-count").textContent = T.plants(shown);
    $("spg-empty").hidden = shown > 0;
  }

  function renderIndications() {
    var ts = terms(), map = {};
    plants.forEach(function (p) {
      if (state.planet && p.pianeti.indexOf(state.planet) < 0) return;
      p.indicazioni.forEach(function (i) {
        (map[i] = map[i] || []).push(p);
      });
    });
    var keys = Object.keys(map).sort(function (a, b) { return a.localeCompare(b, "it"); });
    if (ts.length) {
      keys = keys.filter(function (k) {
        var hay = fold(k + " " + map[k].map(function (p) { return p.nome + " " + (p.nomeef || ""); }).join(" "));
        return ts.every(function (t) { return hay.indexOf(t) >= 0; });
      });
    }
    var h = [], letter = null;
    keys.forEach(function (k) {
      var l = fold(k).charAt(0).toUpperCase();
      if (l !== letter) {
        if (letter !== null) h.push("</dl>");
        h.push('<h3 class="spg-letter">' + l + '</h3><dl class="spg-ind">');
        letter = l;
      }
      var ps = map[k].slice().sort(function (a, b) { return a.nome.localeCompare(b.nome, "it"); });
      h.push("<dt>" + esc(k) + "</dt><dd>" + ps.map(function (p) {
        return '<a href="#' + p._id + '" data-goto="' + p._id + '">' + esc(p.nome) + "</a>";
      }).join("") + "</dd>");
    });
    if (letter !== null) h.push("</dl>");
    $("spg-indications").innerHTML = h.join("");
    $("spg-count").textContent = T.indications(keys.length);
    $("spg-empty").hidden = keys.length > 0;
  }

  function refresh() {
    $("spg-clear").hidden = !state.q;
    if (state.view === "piante") applyPlantsFilter(); else renderIndications();
  }

  /* ---------- navigation ---------- */

  function setView(v) {
    state.view = v;
    ["piante", "disturbi"].forEach(function (x) {
      $("spg-view-" + x).hidden = x !== v;
      $("spg-tab-" + x).setAttribute("aria-selected", String(x === v));
    });
    $("spg-plant-actions").hidden = v !== "piante";
    refresh();
  }

  function goTo(id, push) {
    var el = $(id);
    if (!el) return;
    if (state.view !== "piante") setView("piante");
    if (el.hidden) { // make sure the target is visible
      state.q = ""; state.planet = null; $("spg-q").value = "";
      setPlanetButtons(); refresh();
    }
    el.open = true;
    if (push && history.replaceState) history.replaceState(null, "", "#" + id);
    var bar = $("spg-toolbar").offsetHeight || 0;
    window.scrollTo({ top: el.getBoundingClientRect().top + window.pageYOffset - bar - 8, behavior: "smooth" });
  }

  function setPlanetButtons() {
    root.querySelectorAll("[data-planet]").forEach(function (b) {
      b.setAttribute("aria-pressed", String((b.getAttribute("data-planet") || null) === state.planet));
    });
  }

  /* ---------- wiring ---------- */

  function i18nStatic() {
    root.querySelectorAll("[data-i18n]").forEach(function (el) { el.textContent = T[el.getAttribute("data-i18n")]; });
    root.querySelectorAll("[data-i18n-placeholder]").forEach(function (el) { el.placeholder = T[el.getAttribute("data-i18n-placeholder")]; });
    root.querySelectorAll("[data-i18n-title]").forEach(function (el) {
      el.title = T[el.getAttribute("data-i18n-title")];
      el.setAttribute("aria-label", el.title);
    });
    root.querySelectorAll("[data-i18n-aria-label]").forEach(function (el) { el.setAttribute("aria-label", T[el.getAttribute("data-i18n-aria-label")]); });
  }

  function wire() {
    var timer;
    $("spg-q").addEventListener("input", function (e) {
      clearTimeout(timer);
      timer = setTimeout(function () { state.q = e.target.value; refresh(); }, 120);
    });
    $("spg-clear").addEventListener("click", function () {
      state.q = ""; $("spg-q").value = ""; refresh(); $("spg-q").focus();
    });
    $("spg-sort").addEventListener("change", function (e) {
      state.sort = e.target.value; renderCards(); refresh();
    });
    $("spg-expand").addEventListener("click", function () {
      root.querySelectorAll(".spg-card:not([hidden])").forEach(function (d) { d.open = true; });
    });
    $("spg-collapse").addEventListener("click", function () {
      root.querySelectorAll(".spg-card").forEach(function (d) { d.open = false; });
    });
    root.querySelectorAll(".spg-tab").forEach(function (b) {
      b.addEventListener("click", function () { setView(b.getAttribute("data-view")); });
    });

    root.addEventListener("click", function (e) {
      var t = e.target.closest ? e.target.closest("[data-planet],[data-goto],[data-search],[data-copy]") : null;
      if (!t) return;
      if (t.hasAttribute("data-planet")) {
        state.planet = t.getAttribute("data-planet") || null;
        setPlanetButtons(); refresh();
      } else if (t.hasAttribute("data-goto")) {
        e.preventDefault(); goTo(t.getAttribute("data-goto"), true);
      } else if (t.hasAttribute("data-search")) { // click on an indication → list of plants for it
        state.q = t.getAttribute("data-search"); $("spg-q").value = state.q;
        setView("disturbi");
        $("spg-toolbar").scrollIntoView({ behavior: "smooth" });
      } else if (t.hasAttribute("data-copy")) {
        e.preventDefault();
        var url = location.href.split("#")[0] + "#" + t.getAttribute("data-copy");
        if (history.replaceState) history.replaceState(null, "", "#" + t.getAttribute("data-copy"));
        if (navigator.clipboard) {
          navigator.clipboard.writeText(url).then(function () {
            var old = t.textContent; t.textContent = "✓ " + T.copied;
            setTimeout(function () { t.textContent = old; }, 1500);
          }, function () {});
        }
      }
    });

    // Keep the URL in sync with the opened card (shareable links).
    root.addEventListener("toggle", function (e) {
      var d = e.target;
      if (d.classList && d.classList.contains("spg-card") && d.open && history.replaceState) {
        history.replaceState(null, "", "#" + d.id);
      }
    }, true);

    window.addEventListener("hashchange", function () {
      var id = decodeURIComponent(location.hash.slice(1));
      if (id && $(id)) goTo(id, false);
    });

    var top = $("spg-top");
    window.addEventListener("scroll", function () {
      top.hidden = window.pageYOffset < 600;
    }, { passive: true });
    top.addEventListener("click", function () {
      root.scrollIntoView({ behavior: "smooth" });
    });
  }

  /* ---------- init ---------- */

  i18nStatic();
  wire();

  fetch("/static/plants.json")
    .then(function (r) { return r.json(); })
    .then(function (data) {
      data.forEach(function (p) { p._preps = (p.preparazioni || []).filter(published); });
      // Plants with no published preparation are not shown.
      plants = data.filter(function (p) { return p._preps.length > 0; });
      var seen = {};
      plants.forEach(function (p) {
        var id = slug(p.nome) || "pianta";
        while (seen[id]) id += "-";
        seen[id] = true;
        p._id = id;
        p.pianeti = p.pianeti || []; p.segni = p.segni || []; p.indicazioni = p.indicazioni || [];
        p._fold = {};
        SEARCH_FIELDS.forEach(function (f) { p._fold[f] = fold(fieldText(p, f)); });
        p._hay = SEARCH_FIELDS.map(function (f) { return p._fold[f]; }).join(" ") + " " +
                 fold(p.pianeti.join(" ") + " " + p.segni.join(" ") + " " +
                      p._preps.map(function (x) { return x.nome; }).join(" "));
      });
      buildPlanetFilter();
      renderCards();
      refresh();
      var id = decodeURIComponent(location.hash.slice(1));
      if (id && $(id)) setTimeout(function () { goTo(id, false); }, 50);
    })
    .catch(function (err) {
      $("spg-empty").hidden = false;
      $("spg-empty").textContent = "Errore nel caricamento di plants.json: " + err;
    });
})();
