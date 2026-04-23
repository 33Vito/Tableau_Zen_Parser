/* global vis */

const state = {
  xmlDoc: null,
  allCalc: [],
  allParams: new Set(),
  allRaw: [],
  network: null,
  nodes: [],
  edges: [],
};

const dom = {
  fileInput: document.getElementById("fileInput"),
  demoBtn: document.getElementById("demoBtn"),
  randomSeed: document.getElementById("randomSeed"),
  includeRaw: document.getElementById("includeRaw"),
  includeParams: document.getElementById("includeParams"),
  usePhysics: document.getElementById("usePhysics"),
  focusSelect: document.getElementById("focusSelect"),
  resetView: document.getElementById("resetView"),
  status: document.getElementById("status"),
  network: document.getElementById("network"),
  dataSourceTable: document.getElementById("dataSourceTable"),
  excelPathTable: document.getElementById("excelPathTable"),
  csvPathTable: document.getElementById("csvPathTable"),
  calcTable: document.getElementById("calcTable"),
};

function setStatus(message, kind = "info") {
  dom.status.textContent = message || "";
  dom.status.className = `status ${kind}`;
}

function escapeRegex(text) {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function attrMap(node) {
  const obj = {};
  for (const attr of node.attributes || []) {
    obj[attr.name] = attr.value;
  }
  return obj;
}

function uniqueByCaption(items) {
  const seen = new Set();
  return items.filter((item) => {
    const key = item.caption || item.name || JSON.stringify(item);
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

function renderTable(tableEl, rows) {
  tableEl.innerHTML = "";
  if (!rows || rows.length === 0) {
    tableEl.innerHTML = "<tr><td class=\"empty\">No data</td></tr>";
    return;
  }
  const headers = Object.keys(rows[0]);
  const thead = document.createElement("thead");
  const headRow = document.createElement("tr");
  headers.forEach((key) => {
    const th = document.createElement("th");
    th.textContent = key;
    headRow.appendChild(th);
  });
  thead.appendChild(headRow);

  const tbody = document.createElement("tbody");
  rows.forEach((row) => {
    const tr = document.createElement("tr");
    headers.forEach((key) => {
      const td = document.createElement("td");
      td.textContent = row[key] ?? "";
      tr.appendChild(td);
    });
    tbody.appendChild(tr);
  });

  tableEl.appendChild(thead);
  tableEl.appendChild(tbody);
}

function parseTwb(xmlText) {
  const parser = new DOMParser();
  const xml = parser.parseFromString(xmlText, "text/xml");
  if (xml.querySelector("parsererror")) {
    throw new Error("Invalid XML in .twb file.");
  }

  const dataSources = Array.from(xml.querySelectorAll("relation[table]"))
    .map(attrMap);

  const excelConnections = Array.from(
    xml.querySelectorAll("named-connection[name*='excel-direct']")
  ).map((node) => {
    const connection = node.querySelector("connection");
    return { ...attrMap(node), ...attrMap(connection || { attributes: [] }) };
  });

  const csvConnections = Array.from(
    xml.querySelectorAll("connection[class*='textscan']")
  ).map(attrMap);

  const params = new Set(
    Array.from(xml.querySelectorAll("column[param-domain-type]"))
      .map((node) => node.getAttribute("caption"))
      .filter(Boolean)
  );

  const rawVars = Array.from(xml.querySelectorAll("column[name]"))
    .filter((node) => !node.querySelector("calculation"))
    .map((node) => {
      const attrs = attrMap(node);
      let caption = attrs.caption;
      if (!caption && attrs.name) {
        caption = attrs.name.replace(/[\[\]]/g, "");
      }
      return {
        ...attrs,
        caption,
        formula: "",
      };
    });

  const calcCols = Array.from(xml.querySelectorAll("column[caption]"))
    .filter((node) => node.querySelector("calculation"))
    .map((node) => {
      const attrs = attrMap(node);
      const calc = node.querySelector("calculation");
      const calcAttrs = attrMap(calc || { attributes: [] });
      return {
        ...attrs,
        ...calcAttrs,
      };
    });

  const allCalc = uniqueByCaption(calcCols);

  return {
    dataSources,
    excelConnections,
    csvConnections,
    params,
    rawVars,
    allCalc,
  };
}

function normalizeFormulas(allCalc) {
  const updated = allCalc.map((item) => ({ ...item }));
  for (let i = 0; i < updated.length; i += 1) {
    const name = updated[i].name;
    const caption = updated[i].caption;
    if (!name || !caption) continue;
    const pattern = new RegExp(escapeRegex(name), "g");
    for (let j = 0; j < updated.length; j += 1) {
      if (!updated[j].formula) continue;
      updated[j].formula = updated[j].formula.replace(pattern, `[${caption}]`);
    }
  }
  return updated;
}

function buildNetwork({ allCalc, params, rawVars }) {
  const inputCaptions = allCalc.map((item) => item.caption).filter(Boolean);
  const inputTokens = inputCaptions.map((caption) => `[${caption}]`);

  const usageCount = new Map();
  const edges = [];

  for (let i = 0; i < allCalc.length; i += 1) {
    const formula = allCalc[i].formula || "";
    for (let j = 0; j < inputTokens.length; j += 1) {
      if (i === j) continue;
      if (formula.includes(inputTokens[j])) {
        edges.push({ from: j + 1, to: i + 1, length: 1 });
        usageCount.set(inputCaptions[j], (usageCount.get(inputCaptions[j]) || 0) + 1);
      }
    }
  }

  const nodes = allCalc.map((item, index) => {
    const caption = item.caption || "(unknown)";
    const role = item.role;
    const datatype = item.datatype;

    let group = "calculated field";
    let color = "#67BF5C";

    if (params.has(caption)) {
      group = "parameter";
      color = "#FF9E4A";
    } else if (rawVars.find((raw) => raw.caption === caption)) {
      group = "raw_variable";
      color = "#AD8BC9";
    } else if (role === "dimension") {
      color = "#729ECE";
    }

    let shape = "dot";
    if (datatype === "string") shape = "square";
    if (datatype === "boolean") shape = "triangle";
    if (datatype === "date" || datatype === "datetime") shape = "box";

    // Use a uniform node size for a consistent visual scale.
    const fixedSize = 10;
    const title = `${caption} (${datatype || "unknown"})<br><br>${(item.formula || "")
      .replace(/\n/g, "<br>")
      .replace(/\/\//g, "<br>//")}`;

    return {
      id: index + 1,
      label: caption,
      size: fixedSize,
      group,
      color,
      shape,
      title,
    };
  });

  // Match original Shiny behavior: keep only nodes that participate in at least
  // one dependency edge, so disconnected raw dimensions do not clutter the view.
  const connectedNodeIds = new Set();
  edges.forEach((edge) => {
    connectedNodeIds.add(edge.from);
    connectedNodeIds.add(edge.to);
  });
  const filteredNodes = nodes.filter((node) => connectedNodeIds.has(node.id));

  return { nodes: filteredNodes, edges };
}

function updateFocusOptions(nodes) {
  const current = dom.focusSelect.value;
  dom.focusSelect.innerHTML = "<option value=\"\">(none)</option>";
  nodes.forEach((node) => {
    const option = document.createElement("option");
    option.value = node.id;
    option.textContent = node.label;
    dom.focusSelect.appendChild(option);
  });
  dom.focusSelect.value = current;
}

function renderNetwork() {
  if (!state.xmlDoc) return;

  const includeRaw = dom.includeRaw.checked;
  const includeParams = dom.includeParams.checked;

  let allCalc = [...state.allCalc];
  if (includeRaw) {
    allCalc = allCalc.concat(state.allRaw);
  }
  if (!includeParams) {
    allCalc = allCalc.filter((item) => !state.allParams.has(item.caption));
  }

  allCalc = normalizeFormulas(allCalc);

  const { nodes, edges } = buildNetwork({
    allCalc,
    params: state.allParams,
    rawVars: state.allRaw,
  });

  state.nodes = nodes;
  state.edges = edges;

  const data = {
    nodes: new vis.DataSet(nodes),
    edges: new vis.DataSet(edges),
  };

  const options = {
    layout: {
      randomSeed: Number(dom.randomSeed.value) || 3,
    },
    nodes: {
      font: { size: 10 },
      scaling: { min: 8, max: 40 },
      shadow: true,
    },
    edges: {
      arrows: "to",
      color: { opacity: 0.6 },
    },
    interaction: {
      hover: true,
      multiselect: true,
      selectConnectedEdges: true,
      hoverConnectedEdges: true,
    },
    physics: dom.usePhysics.checked,
  };

  if (state.network) {
    state.network.destroy();
  }
  state.network = new vis.Network(dom.network, data, options);

  updateFocusOptions(nodes);
}

function renderTables({ dataSources, excelConnections, csvConnections, allCalc }) {
  renderTable(dom.dataSourceTable, dataSources);
  renderTable(dom.excelPathTable, excelConnections);
  renderTable(dom.csvPathTable, csvConnections);
  renderTable(dom.calcTable, allCalc);
}

function loadParsedData(parsed) {
  state.xmlDoc = true;
  state.allCalc = parsed.allCalc;
  state.allParams = parsed.params;
  state.allRaw = parsed.rawVars;

  renderTables(parsed);
  renderNetwork();

  setStatus(
    `Loaded workbook with ${parsed.allCalc.length} calculated fields.`,
    "success"
  );
}

async function loadDemo() {
  setStatus("Loading demo workbook...", "info");
  const response = await fetch("Ward Population Pyramid - Quinary Age.twb");
  if (!response.ok) {
    throw new Error("Failed to load demo workbook. Serve this folder via HTTP.");
  }
  const text = await response.text();
  const parsed = parseTwb(text);
  loadParsedData(parsed);
}

async function handleFile(file) {
  setStatus("Parsing workbook...", "info");
  const text = await file.text();
  const parsed = parseTwb(text);
  loadParsedData(parsed);
}

function focusOnNode(nodeId) {
  if (!state.network || !nodeId) return;
  state.network.selectNodes([Number(nodeId)]);
  state.network.focus(Number(nodeId), {
    scale: 1.3,
    animation: { duration: 500 },
  });
}

function resetView() {
  if (!state.network) return;
  state.network.fit({ animation: { duration: 400 } });
  state.network.unselectAll();
  dom.focusSelect.value = "";
}

function bindEvents() {
  dom.demoBtn.addEventListener("click", async () => {
    try {
      await loadDemo();
    } catch (error) {
      setStatus(error.message, "error");
    }
  });

  dom.fileInput.addEventListener("change", async (event) => {
    const file = event.target.files[0];
    if (!file) return;
    try {
      await handleFile(file);
    } catch (error) {
      setStatus(error.message, "error");
    }
  });

  [dom.randomSeed, dom.includeRaw, dom.includeParams, dom.usePhysics].forEach((el) => {
    el.addEventListener("change", () => {
      try {
        renderNetwork();
      } catch (error) {
        setStatus(error.message, "error");
      }
    });
  });

  dom.focusSelect.addEventListener("change", (event) => {
    focusOnNode(event.target.value);
  });

  dom.resetView.addEventListener("click", resetView);
}

bindEvents();
setStatus("Upload a .twb file or load the demo workbook.", "info");
