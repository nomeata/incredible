var translations = {
  de: { translation: {
    "What do you want to prove today?": "Was möchtest du gerne beweisen?",
    "Custom tasks": "Eigene Aufgaben",
    "Add": "Hinzufügen",
    "Current task:": "Aktuelle Aufgabe:",
    "Logics blocks:": "Logikbausteine:",
    "Helper blocks:": "Hilfsbausteine:",
    "Custom blocks:": "Eigene Bausteine:",
    "Developers tools:": "Entwicklertools",
    "You proved:": "Du hast bewiesen:",
    "switch task...": "Aufgabe wechseln",
    "confirm-reset": "Willst du wirklich alle gespeicherten Daten löschen (Beweise, eigene Aufgaben, eigene Bausteine)?",
    "nothing": "nichts",
    "task-parse-error": "Leider verstehe ich die Aufgabe nicht.",
    "Input proposition": "Bitte gebe die Aussage ein",
    "Could not parse, please try again:": "Konnte die Aussage nicht verstehen, probiere es nochmal:",
  }},
  en: { translation: {
    "confirm-reset": "Are you sure you want to remove all saved data (proofs, custom tasks, custom blocks)?",
    "nothing": "nothing",
    "task-parse-error": "Sorry, I could not understand this task.",
  }},
};


i18n.init({
    resStore: translations,
    fallbackLng: 'en',
    debug: true,
    nsseparator: "::unused::",
    keyseparator: "::unused::",
  }, function(err, t) {
  $("body").i18n();
});
