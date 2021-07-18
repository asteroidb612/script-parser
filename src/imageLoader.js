//  ___
// |_ _|_ __ ___   __ _  __ _  ___
//  | || '_ ` _ \ / _` |/ _` |/ _ \
//  | || | | | | | (_| | (_| |  __/
// |___|_| |_| |_|\__,_|\__, |\___|
//                      |___/
//  _                    _
// | |    ___   __ _  __| | ___ _ __
// | |   / _ \ / _` |/ _` |/ _ \ '__|
// | |__| (_) | (_| | (_| |  __/ |
// |_____\___/ \__,_|\__,_|\___|_|
const Tesseract = require("tesseract.js");

class ImageLoader extends HTMLElement {
  constructor() {
    super();
    this.processFile = this.processFile.bind(this);
  }

  processFile(evt) {
    const selectedFiles = evt.target.files;
    const finished = {};
    for (let i = 0; i < selectedFiles.length; i++) {
      const file = selectedFiles.item(i);
      const filename = file.name;
      const reader = new FileReader();

      reader.onload = async (e) => {
        const data = e.target.result;
        const { createWorker } = Tesseract;
        const worker = createWorker({
          logger: (m) => console.log(m),
        });
        await worker.load();
        await worker.loadLanguage("eng");
        await worker.initialize("eng");
        const {
          data: { text },
        } = await worker.recognize(file);
        finished[filename] = text;
        if (Object.keys(finished).length === selectedFiles.length) {
          const pages = Object.entries(finished).sort(([name, text]) => name);
          const script = pages.reduce((acc, [name, text]) => {
            acc += "\n";
            acc += text;
            return acc;
          }, "");
          const event = new CustomEvent("image-loader-finished", {
            detail: script,
          });
          this.dispatchEvent(event);
        }
        await worker.terminate();
      };
      reader.readAsArrayBuffer(file);
    }
  }

  connectedCallback() {
    const input = document.createElement("input");
    input.type = "file";
    input.multiple = true;
    input.addEventListener("change", this.processFile);
    input.innerHtml = "Open file";
    this.appendChild(input);
  }
}
customElements.define("custom-file", ImageLoader);
