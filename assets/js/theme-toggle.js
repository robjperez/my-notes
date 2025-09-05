// Theme toggle functionality
(function() {
    'use strict';
    
    // Initialize theme on page load
    function initTheme() {
        const savedTheme = localStorage.getItem('theme');
        const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
        
        // Determine initial theme
        let theme;
        if (savedTheme) {
            theme = savedTheme;
        } else {
            theme = prefersDark ? 'dark' : 'light';
        }
        
        // Apply theme
        document.documentElement.setAttribute('data-theme', theme);
        
        // Create and add toggle button
        createToggleButton();
    }
    
    // Create the toggle button
    function createToggleButton() {
        // Check if button already exists
        if (document.querySelector('.theme-toggle')) {
            return;
        }
        
        const toggleButton = document.createElement('div');
        toggleButton.className = 'theme-toggle';
        toggleButton.setAttribute('aria-label', 'Toggle dark/light mode');
        toggleButton.setAttribute('role', 'button');
        toggleButton.setAttribute('tabindex', '0');
        
        const slider = document.createElement('div');
        slider.className = 'theme-toggle-slider';
        toggleButton.appendChild(slider);
        
        // Add click event
        toggleButton.addEventListener('click', toggleTheme);
        
        // Add keyboard support
        toggleButton.addEventListener('keydown', function(e) {
            if (e.key === 'Enter' || e.key === ' ') {
                e.preventDefault();
                toggleTheme();
            }
        });
        
        // Add to page
        document.body.appendChild(toggleButton);
    }
    
    // Toggle between themes
    function toggleTheme() {
        const currentTheme = document.documentElement.getAttribute('data-theme');
        const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
        
        // Apply new theme
        document.documentElement.setAttribute('data-theme', newTheme);
        
        // Save to localStorage
        localStorage.setItem('theme', newTheme);
        
        // Add a subtle animation effect
        document.body.style.transition = 'background-color 0.3s ease, color 0.3s ease';
        setTimeout(() => {
            document.body.style.transition = '';
        }, 300);
    }
    
    // Listen for system theme changes
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function(e) {
        // Only update if user hasn't manually set a preference
        if (!localStorage.getItem('theme')) {
            const newTheme = e.matches ? 'dark' : 'light';
            document.documentElement.setAttribute('data-theme', newTheme);
        }
    });
    
    // Initialize when DOM is ready
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initTheme);
    } else {
        initTheme();
    }
})();
