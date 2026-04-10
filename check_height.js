const fs = require('fs');
const jsdom = require('jsdom');
const { JSDOM } = jsdom;

const css = fs.readFileSync('Resources/shared.css', 'utf-8');

const pages = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const results = [];

pages.forEach(day => {
    const html = fs.readFileSync(`Resources/D${day}/index.html`, 'utf-8');
    // Using jsdom, but jsdom won't calculate actual rendered height accurately without a real layout engine.
    // However, we can check if the DOM structure or classes are identical.
    const dom = new JSDOM(html);
    const document = dom.window.document;
    const header = document.querySelector('.page-header');
    
    // Get text contents to calculate length
    const h1 = header.querySelector('h1').textContent;
    const sub = header.querySelector('.subtitle').textContent;
    
    results.push({
        day,
        h1Length: h1.length,
        subLength: sub.length,
        h1Text: h1,
        subText: sub
    });
});

console.log(JSON.stringify(results, null, 2));
