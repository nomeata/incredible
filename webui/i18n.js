var translations = {
  de: { translation: {
    "What do you want to prove today?": "Was möchtest du gerne beweisen?",
    "Custom tasks": "Eigene Aufgaben",
    "Add": "Hinzufügen",
    "Current task:": "Aktuelle Aufgabe:",
    "Logic blocks:": "Logikbausteine:",
    "Helper blocks:": "Hilfsbausteine:",
    "Custom blocks:": "Eigene Bausteine:",
    "Developers tools:": "Entwicklertools",
    "New custom block:": "Neuer eigener Baustein:",
    "switch": "Eine andere Aufgabe bearbeiten...",
    "confirm-reset": "Willst du wirklich alle gespeicherten Daten löschen (Beweise, eigene Aufgaben, eigene Bausteine)?",
    "nothing": "nichts",
    "task-parse-error": "Leider verstehe ich die Aufgabe nicht.",
    "Input proposition": "Bitte gebe die Aussage ein",
    "Could not parse, please try again:": "Konnte die Aussage nicht verstehen, probiere es nochmal:",
    "Session 1": "Lektion 1",
    "Session 2": "Lektion 2",
    "Session 3": "Lektion 3",
    "Session 4": "Lektion 4",
    "Session 5": "Lektion 5",
    "Session 6": "Lektion 6",
    "Session 7": "Lektion 7",
    "Session 8": "Lektion 8",
    "Session 9": "Lektion 9",
    "Hilbert system": "Hilbert-Kalkül",
    "NAND calculus": "NAND-Kalkül",
    "Help": "Hilfe",
    "Save proof as SVG image.": "Den Beweis als SVG-Datei speichern.",
    "Forget all stored data": "Alle gespeicherten Daten löschen.",
    "Reset this task": "Diesen Task neu beginnen",
    "Unlock": "Entsperren",
    "password": "Passwort",
    "wrong-password": "Das ist leider nicht das richtige Passwort.",
    "add-title": "Eine neue Aufgabe hinzufügen.",
    "Block count:": "Bausteinzähler",
    "Your proof block count. Try to minimize it!": "So viele Beweisblöcke verwendest du. Geht es auch mit weniger?",
    "The best known proof count. Did you beat it? Let us know!": "Das ist die beste bekannte Blockanzahl. Du hast es mit weniger geschafft? Lass es uns wissen!"
  }},

  en: { translation: {
    "confirm-reset": "Are you sure you want to remove all saved data (proofs, custom tasks, custom blocks)?",
    "nothing": "nothing",
    "switch": "switch task...",
    "task-parse-error": "Sorry, I could not understand this task.",
    "wrong-password": "Sorry, this is not the right password",
    "add-title": "Add a new task to this session."
  }}
};


i18n.init({
    resStore: translations,
    fallbackLng: 'en',
    debug: true,
    nsseparator: "::unused::",
    keyseparator: "::unused::",
    // lng: 'en', // hardcoding, for screenshots
  }, function(err, t) {
  $("body").i18n();
});
