// Dynamic tag styling system
// Generates consistent colors for tags based on their names

(function() {
    'use strict';

    // Color palettes for light and dark modes
    const lightColors = [
        { bg: '#f0f4f0', color: '#4a6741', border: '#6b8e6b' }, // green
        { bg: '#f5f0f0', color: '#6b4a4a', border: '#8b6b6b' }, // red
        { bg: '#f0f2f5', color: '#4a5a6b', border: '#6b7b8b' }, // blue
        { bg: '#f5f2f0', color: '#6b5a4a', border: '#8b7b6b' }, // orange
        { bg: '#f0f5f5', color: '#4a6b6b', border: '#6b8b8b' }, // teal
        { bg: '#f5f5f0', color: '#6b6b4a', border: '#8b8b6b' }, // yellow
        { bg: '#f2f0f5', color: '#5a4a6b', border: '#7b6b8b' }, // purple
        { bg: '#f0f0f5', color: '#4a4a6b', border: '#6b6b8b' }, // indigo
        { bg: '#f5f0f2', color: '#6b4a5a', border: '#8b6b7b' }, // pink
        { bg: '#f0f5f2', color: '#4a6b5a', border: '#6b8b7b' }, // mint
    ];

    const darkColors = [
        { bg: '#2a3a2a', color: '#8ba68b', border: '#5a7a5a' }, // green
        { bg: '#3a2a2a', color: '#a68b8b', border: '#7a5a5a' }, // red
        { bg: '#2a323a', color: '#8b9aa6', border: '#5a6a7a' }, // blue
        { bg: '#3a322a', color: '#a69a8b', border: '#7a6a5a' }, // orange
        { bg: '#2a3a3a', color: '#8ba6a6', border: '#5a7a7a' }, // teal
        { bg: '#3a3a2a', color: '#a6a68b', border: '#7a7a5a' }, // yellow
        { bg: '#322a3a', color: '#9a8ba6', border: '#6a5a7a' }, // purple
        { bg: '#2a2a3a', color: '#8b8ba6', border: '#5a5a7a' }, // indigo
        { bg: '#3a2a32', color: '#a68b9a', border: '#7a5a6a' }, // pink
        { bg: '#2a3a32', color: '#8ba69a', border: '#5a7a6a' }, // mint
    ];

    // Simple hash function to convert string to number
    function hashString(str) {
        let hash = 0;
        for (let i = 0; i < str.length; i++) {
            const char = str.charCodeAt(i);
            hash = ((hash << 5) - hash) + char;
            hash = hash & hash; // Convert to 32-bit integer
        }
        return Math.abs(hash);
    }

    // Get color scheme based on current theme
    function getCurrentColorScheme() {
        const isDark = document.documentElement.getAttribute('data-theme') === 'dark' ||
                      (!document.documentElement.getAttribute('data-theme') && 
                       window.matchMedia('(prefers-color-scheme: dark)').matches);
        return isDark ? darkColors : lightColors;
    }

    // Generate CSS for a tag
    function generateTagCSS(tagName, colorScheme) {
        const hash = hashString(tagName);
        const colorIndex = hash % colorScheme.length;
        const colors = colorScheme[colorIndex];
        
        return `
            .tag-${tagName} {
                background: ${colors.bg} !important;
                color: ${colors.color} !important;
                border-color: ${colors.border} !important;
            }`;
    }

    // Apply dynamic styling to all tags
    function styleTags() {
        const tags = document.querySelectorAll('.tag');
        const colorScheme = getCurrentColorScheme();
        const processedTags = new Set();
        
        // Create a style element if it doesn't exist
        let styleElement = document.getElementById('dynamic-tag-styles');
        if (!styleElement) {
            styleElement = document.createElement('style');
            styleElement.id = 'dynamic-tag-styles';
            document.head.appendChild(styleElement);
        }
        
        let css = '';
        
        tags.forEach(tag => {
            const classList = Array.from(tag.classList);
            const tagClass = classList.find(cls => cls.startsWith('tag-') && cls !== 'tag');
            
            if (tagClass) {
                const tagName = tagClass.replace('tag-', '');
                if (!processedTags.has(tagName)) {
                    css += generateTagCSS(tagName, colorScheme);
                    processedTags.add(tagName);
                }
            }
        });
        
        styleElement.textContent = css;
    }

    // Re-style tags when theme changes
    function handleThemeChange() {
        styleTags();
    }

    // Initialize when DOM is ready
    function init() {
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', styleTags);
        } else {
            styleTags();
        }
        
        // Listen for theme changes
        const themeToggle = document.querySelector('.theme-toggle');
        if (themeToggle) {
            themeToggle.addEventListener('click', () => {
                setTimeout(handleThemeChange, 100); // Small delay to let theme change
            });
        }
        
        // Listen for system theme changes
        window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', handleThemeChange);
    }

    // Start the system
    init();
})();
