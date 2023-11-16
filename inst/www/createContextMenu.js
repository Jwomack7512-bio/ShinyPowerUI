// contextMenu.js

function createContextMenu(containerId, menuItems, itemPadding = '5px', itemFontSize = '14px', menuWidth = 'auto') {
    var container = document.getElementById(containerId);
    var customContextMenu;

    function removeContextMenu() {
        if (customContextMenu && container.contains(customContextMenu)) {
            container.removeChild(customContextMenu);
            document.removeEventListener('mouseup', handleDocumentClick);
        }
    }

    function handleDocumentClick(e) {
        // If the click is outside the container or the context menu, remove the context menu
        if (!customContextMenu.contains(e.target)) {
            removeContextMenu();
        }
    }

    container.addEventListener('contextmenu', function (event) {
        // Prevent the default context menu
        event.preventDefault();

        // Check if a context menu is already open, close it
        removeContextMenu();

        // Create a div element for the custom context menu
        customContextMenu = document.createElement('div');
        customContextMenu.style.position = 'absolute';
        customContextMenu.style.left = event.pageX + 'px';
        customContextMenu.style.top = event.pageY + 'px';
        customContextMenu.style.backgroundColor = '#ffffff';
        customContextMenu.style.border = '1px solid #cccccc';
        customContextMenu.style.padding = itemPadding;
        customContextMenu.style.width = menuWidth;
        customContextMenu.style.zIndex = '1000';
        // Add shadow and rounded corners for a modern look
        customContextMenu.style.boxShadow = '0 2px 5px rgba(0, 0, 0, 0.2)';
        customContextMenu.style.borderRadius = '4px';

        // Create clickable elements and add them to the custom context menu
        menuItems.forEach(function (menuItem) {
            var option = createOption(menuItem.name, menuItem.id);
            customContextMenu.appendChild(option);
        });

        // Append the custom context menu to the container
        container.appendChild(customContextMenu);

        // Add mouseup event listener to remove the custom context menu when clicked outside
        document.addEventListener('mouseup', handleDocumentClick);

        // Function to create a clickable option
        function createOption(name, id) {
            var option = document.createElement('div');
            option.innerHTML = name;
            option.style.cursor = 'pointer';

            // Set the padding and font size based on function parameters
            option.style.padding = itemPadding;
            option.style.fontSize = itemFontSize;

            option.addEventListener('click', function () {
                console.log(name + ' clicked! ID: ' + id);
                // Add your logic for the clicked option here
                // You can use the 'id' parameter to identify the clicked item
                Shiny.setInputValue(containerId + "_" + id, true);
                // Remove the context menu
                removeContextMenu();
            });

            // Highlight the option on hover
            option.addEventListener('mouseenter', function () {
                option.style.backgroundColor = '#e0e0e0'; // Change the background color on hover
            });

            // Remove the highlight when the mouse leaves the option
            option.addEventListener('mouseleave', function () {
                option.style.backgroundColor = ''; // Reset the background color when not hovered
            });

            return option;
        }
    });
}
