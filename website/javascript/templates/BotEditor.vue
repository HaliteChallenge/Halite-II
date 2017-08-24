<template>
    <div class="editorArea">
        <div v-if="editorViewer">
            <span class="optGroup">
                Editor Theme:
                <select v-model="selected_theme" v-on:change="reset_theme">
                    <option>Dark</option>
                    <option>Light</option>
                </select>
            </span>
            <span class="optGroup">
                Bot Language:
                <select v-model="selected_language" v-on:change="reset_language">
                    <option v-for="(_, lang) in all_bot_languages"
                            v-bind:value="lang">
                    {{ lang }}
                    </option>
                </select>
            </span>
            <span class="optGroup">
                <button v-on:click="reset_code">Reset Code</button>
            </span>
        </div>
        <div class="editorBody" id="embeddedEditor">
            <div class="editorTitle">
                <span id = "progressMessageDiv">Loading language tooling plugins...</span>
            </div>
        </div>
        <div class="bot-code-controls" v-if="editorViewer">
            <button v-on:click="download_bot">Download Bot Code</button>
            <span v-if="logged_in">
                <button v-on:click="upload_bot">Upload my bot!</button>
                <span v-if="status_message">{{ status_message }}</span>
            </span>
            <span v-else>Log In to upload bot</span>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";

    const botLanguagePacks = {
        'Python3': {
            mimeType: "text/x-python",
            fileName: "MyBot.py",
            zipName: "my-py3-bot.zip",
            starterZipPath: "/assets/downloads/Halite2_Python3_None.zip",
        },
        'Java': {
            mimeType: "text/x-java-source",
            fileName: "MyBot.java",
            zipName: "my-java-bot.zip",
            starterZipPath: "/assets/downloads/Halite2_Java_None.zip",
        },
        'C++': {
            mimeType: "text/x-c++src",
            fileName: "MyBot.cpp",
            zipName: "my-c++-bot.zip",
            starterZipPath: "/assets/downloads/Halite2_C++_None.zip",
        },
    };

    var logVerbose = false;
    const BOT_LANG_KEY = 'bot_language';
    const THEME_KEY = 'editor_theme';
    const DARK_THEME = 'Dark';
    const RESET_MSG = "Are you sure you want to reset your bot code to the default sample code?\n(All changes will be lost!)";

    function saveCode(ctx) {
        const fileName = ctx.bot_info().fileName;
        logInfo("Saving bot file to web local storage: " + fileName);
        const code = ctx.get_editor_code();
        window.localStorage.setItem(fileName, code);
        window.localStorage.setItem(BOT_LANG_KEY, ctx.bot_lang);
        window.localStorage.setItem(THEME_KEY, ctx.selected_theme);
    }

    function loadBotLang() {
        const default_lang = 'Python3';
        var lang = window.localStorage.getItem(BOT_LANG_KEY);
        if (!lang) {
            lang = default_lang;
        }
        logInfo("Setting editor language to " + lang);
        return lang;
    }

    function loadEditorTheme() {
        const theme = window.localStorage.getItem(THEME_KEY);
        if (!theme) {
            theme = DARK_THEME;
        }
        logInfo("Setting editor theme to " + theme);
        return theme;
    }

    function loadCode(ctx) {
        const fileName = ctx.bot_info().fileName;
        logInfo("Loading code into editor from web local storage: " + fileName);
        return window.localStorage.getItem(fileName);
    }

    function logError(err) {
        console.error(err);
    }

    function logInfo(msg) {
        if (logVerbose) console.log(msg);
    }

    function copyZip(zipPromise) {
        return zipPromise.then((zip) => zip.generateAsync({type: 'blob'}))
               .then(JSZip.loadAsync);
    }

    export default {
        name: "bot_editor",
        data: function() {
            const lang = loadBotLang();
            const theme = loadEditorTheme();
            return {
                all_bot_languages: botLanguagePacks,
                status_message: null,
                logged_in: false,
                editorViewer: null,
                // Currently defaulting to Python3 starter kit
                bot_lang: lang,
                selected_language: lang,
                selected_theme: theme,
            };
        },
        mounted: function() {
            api.me().then((me) => {
                if (me !== null) {
                    this.logged_in = true;
                }
            });
            // Restore user's bot code, or use demo code for new bot
            this.get_code_promise().then((code) => this.create_editor(code));
        },
        methods: {
            bot_info: function() {
                return botLanguagePacks[this.bot_lang];
            },
            create_editor: function(code) {
                logInfo("Creating editor...");
                const codeEdit = new orion.codeEdit();
                var opts = {parent: "embeddedEditor", contentType: this.bot_info().mimeType, contents: code};
                codeEdit.create(opts).then((editorViewer) => {

                    logInfo("Initializing editor...");
                    this.editorViewer = editorViewer;
                    const editor = editorViewer.editor;

                    // make sure we're using the correct color theme
                    this.reset_theme();

                    logInfo("Adding editor save handlers...");

                    // Set up save action (note: auto-save is enabled by default)
                    const saveEventHandler = () => {
                        saveCode(this);
                        return true;
                    }

                    // Auto-save event
                    editor.addEventListener("InputChanged", function(evt) {
                        if(evt.contentsSaved) {
                            // save editor contents
                            saveEventHandler();
                            // clear upload status message when user edits code
                            if (this.status_message) {
                                this.status_message = null;
                            }
                        }
                    });

                    // Manual save action (Ctrl+S)
                    editor.getTextView().setAction("save", saveEventHandler);

                    // Be sure to save before closing/leaving the page
                    window.addEventListener("unload", saveEventHandler);

                    // Other settings
                    if (editorViewer.settings) {
                        editorViewer.settings.showOccurrences = true;
                    }

                    logInfo("Editor ready!");
                });
            },
            download_bot: function() {
                this.get_user_zip_promise().then((blob) => {
                    const zipName = this.bot_info().zipName;
                    logInfo("Saving zip file of editor code: " + zipName);
                    saveAs(blob, zipName);
                });
                return true;
            },
            get_code_promise: function() {
                // Restore user's bot code, or use demo code for new bot
                const startCode = loadCode(this);
                return (startCode === null) ? this.get_default_code_promise() : Promise.resolve(startCode);
            },
            get_default_code_promise: function () {
                logInfo("Loading default bot code...");
                const fileName = this.bot_info().fileName;
                return this.get_starter_zip().then((starterZip) => {
                    logInfo("Got starter zip. Getting sample bot file: " + fileName);
                    return starterZip.file(fileName).async('text');
                }, logError);
            },
            get_editor_code: function() {
                return this.editorViewer.editor.getModel().getText();
            },
            get_starter_zip: function fn(forceClean=false) {
                if (fn.cached === undefined) { fn.cached = {}; }
                const lang = this.bot_lang;
                if (fn.cached[lang] === undefined || forceClean) {
                    fn.cached[lang] = new Promise((resolve, reject) => {
                        const starterZipPath = this.bot_info().starterZipPath;
                        JSZipUtils.getBinaryContent(starterZipPath, (err, data) => {
                            logInfo("Getting starter zip: " + starterZipPath);
                            if (err) {
                                reject(err);
                                return;
                            }
                            try {
                                JSZip.loadAsync(data).then(resolve, reject);
                            }
                            catch (e) {
                                reject(e);
                            }
                        });
                    });
                }
                return fn.cached[lang];
            },
            get_user_zip_promise: function() {
                return copyZip(this.get_starter_zip()).then((zip) => {
                    return zip.file(this.bot_info().fileName, this.get_editor_code())
                    .generateAsync({type: 'blob'});
                });
            },
            reload_code: function(use_default=false) {
                const codePromise = use_default ? this.get_default_code_promise() : this.get_code_promise();
                return codePromise.then((contents) => {
                    this.editorViewer.setContents(contents, this.bot_info().mimeType);
                });
            },
            reset_code: function() {
                if (window.confirm(RESET_MSG)) {
                    // reset to starter pack sample code for current language
                    this.reload_code(true);
                }
                return true;
            },
            reset_language: function() {
                saveCode(this); // save code for previous language's bot
                this.bot_lang = this.selected_language;
                return this.reload_code().then(() => {
                    saveCode(this); // save code for new language's bot
                });
            },
            reset_theme: function() {
                const editorElement = jQuery(".textview");
                const darkThemeClass = "editorTheme";
                if (this.selected_theme == DARK_THEME) {
                    editorElement.addClass(darkThemeClass);
                }
                else {
                    editorElement.removeClass(darkThemeClass);
                }
                return true;
            },
            upload_bot: function() {
                this.get_user_zip_promise().then((blob) => {
                    const botFile = new File([blob], this.bot_info().zipName);
                    let user_id;
                    this.status_message = null;

                    const has_bot_promise = api.me().then((user) => {
                        user_id = user.user_id;
                        return api.list_bots(user.user_id);
                    }).then((bots) => {
                        if (bots.length > 0) {
                            return bots[0].bot_id;
                        }
                        return null;
                    });

                    has_bot_promise.then((bot_id) => {
                        logInfo("Uploading zip file: " + botFile);
                        return api.update_bot(user_id, bot_id, botFile, (progress) => {
                            const p = Math.floor(progress * 100);
                            this.status_message = `Uploading... (${p}%)`;
                        });
                    }).then(() => {
                        this.status_message = "Successfully uploaded!";
                    }, (err) => {
                        this.status_message = err.message;
                    });
                });
                return true;
            },
        },
    }
</script>

<style lang="scss" scoped>
    .editorTitle{
        border: none;
        vertical-align: middle;
        overflow: hidden;
        text-align: left;
        margin-left: 15%;
        margin-right: 15%;
        padding-bottom: 5px;
        position: relative;
    }
    .editorBody{
        border: 1px solid;
        vertical-align: middle;
        border-color: black;
        overflow: hidden;
        text-align: left;
        margin: 15px 0px;
        position: relative;
        height: 30em;
    }
    .editorArea {
        margin: 0px 15px;
    }
    .editorArea button, .editorArea select {
        color: black;
    }
    .optGroup {
        margin-right: 1em;
    }
</style>
