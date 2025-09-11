// Prism.js integration for org-mode code blocks
// Converts org-mode src-* classes to language-* classes for Prism.js

(function() {
    'use strict';

    function initPrismOrgMode() {
        // Find all code blocks with org-mode src-* classes
        const codeBlocks = document.querySelectorAll('pre[class*="src-"]');
        
        codeBlocks.forEach(function(block) {
            // Extract the language from the class name
            const classList = Array.from(block.classList);
            const srcClass = classList.find(cls => cls.startsWith('src-'));
            
            if (srcClass) {
                const language = srcClass.replace('src-', '');
                
                // Add the language-* class for Prism.js
                block.classList.add('language-' + language);
                
                // Wrap the content in a <code> element if it doesn't exist
                if (!block.querySelector('code')) {
                    const code = document.createElement('code');
                    code.className = 'language-' + language;
                    code.textContent = block.textContent;
                    block.innerHTML = '';
                    block.appendChild(code);
                }
            }
        });
        
        // Trigger Prism highlighting
        if (typeof Prism !== 'undefined') {
            Prism.highlightAll();
        }
    }

    // Initialize when DOM is ready
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initPrismOrgMode);
    } else {
        initPrismOrgMode();
    }
})();
