const observer = new MutationObserver((mutationList) => {
		confetti.start();
		//setTimeout(confetti.toggle(), 3000000);
		setTimeout(confetti.stop(), 500000000000);
});
const div_element = document.getElementById('comparativa_global') // elemento que queremos observar
// Opcions para el observer
const observerOptions = {
	attributes: true,
	childList: true,
	subtree: true,
	characterData: false,
	attributeOldValue: false,
	characterDataOldValue: false
};
observer.observe(div_element, observerOptions);
