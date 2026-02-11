// API Configuration
const API_BASE = '/api';

// State
let allCourses = [];
let completedCourses = [];

// Initialize app
document.addEventListener('DOMContentLoaded', () => {
    initializeTabs();
    loadCourses();
    setupEventListeners();
});

// Tab functionality
function initializeTabs() {
    const tabButtons = document.querySelectorAll('.tab-button');
    const tabContents = document.querySelectorAll('.tab-content');

    tabButtons.forEach(button => {
        button.addEventListener('click', () => {
            const tabName = button.dataset.tab;

            tabButtons.forEach(btn => btn.classList.remove('active'));
            tabContents.forEach(content => content.classList.remove('active'));

            button.classList.add('active');
            document.getElementById(tabName).classList.add('active');
        });
    });
}

// Setup event listeners
function setupEventListeners() {
    document.getElementById('path-btn').addEventListener('click', getPrerequisitePath);
}

// Load all courses from API
async function loadCourses() {
    try {
        const response = await fetch(`${API_BASE}/courses`);
        const data = await response.json();
        allCourses = data.courses;

        console.log('Loaded courses:', allCourses.length);

        // Pre-select first year courses
        completedCourses = allCourses
            .filter(c => c.course.startsWith('cs11') || ['gmath', 'gart', 'grph', 'fithw', 'cfe101'].includes(c.course))
            .map(c => c.course);

        renderAllCourses();
        await updatePlanner(); // Make sure this completes
    } catch (error) {
        console.error('Error loading courses:', error);
        showError('Failed to load courses. Make sure the Prolog server is running.');
    }
}

// Load all elective courses from API
async function loadElectives() {
    try {
        const response = await fetch(`${API_BASE}/electives`);
        const data = await response.json();
        renderElectives(data.electives);
    } catch (error) {
        console.error('Error loading electives:', error);
    }
}

// Render all courses in the "All Courses" tab
function renderAllCourses() {
    const container = document.getElementById('all-courses-list');
    container.innerHTML = '';

    allCourses.forEach(course => {
        const courseItem = document.createElement('div');
        courseItem.className = 'course-item';

        const prereqText = course.prerequisites.length > 0
            ? `Prerequisites: ${course.prerequisites.map(p => p.toUpperCase()).join(', ')}`
            : 'No prerequisites';

        courseItem.innerHTML = `
            <h3>${course.course.toUpperCase()}</h3>
            <p>${prereqText}</p>
        `;

        container.appendChild(courseItem);
    });
}

// Render available courses in planner (only courses with NO prerequisites)
function renderAvailableCourses() {
    const container = document.getElementById('available-courses');
    container.innerHTML = '';

    // Only show courses that have no prerequisites
    const noPrerequsiteCourses = allCourses.filter(course => course.prerequisites.length === 0);

    if (noPrerequsiteCourses.length === 0) {
        container.innerHTML = '<p class="info-text">No courses found without prerequisites.</p>';
        return;
    }

    noPrerequsiteCourses.forEach(course => {
        const courseCard = document.createElement('div');
        courseCard.className = 'course-card';

        const isCompleted = completedCourses.includes(course.course);
        if (isCompleted) {
            courseCard.classList.add('completed');
        }

        courseCard.innerHTML = `
            <div class="course-code">${course.course.toUpperCase()}</div>
            <div class="course-prereqs">No prerequisites required</div>
        `;

        courseCard.addEventListener('click', () => toggleCourse(course.course));
        container.appendChild(courseCard);
    });
}

// Toggle course completion
function toggleCourse(courseCode) {
    const index = completedCourses.indexOf(courseCode);

    if (index === -1) {
        completedCourses.push(courseCode);
    } else {
        completedCourses.splice(index, 1);
    }

    updatePlanner();
}

// Update the planner view
async function updatePlanner() {
    renderCompletedCourses();
    categorizeCoursesForPlanner();
}

// Render completed courses tracker
function renderCompletedCourses() {
    const container = document.getElementById('completed-courses');
    container.innerHTML = '';

    if (completedCourses.length === 0) {
        container.innerHTML = '<p class="info-text">Click on available courses below to mark them as completed.</p>';
        return;
    }

    completedCourses.forEach(courseCode => {
        const tag = document.createElement('div');
        tag.className = 'completed-tag';

        const span = document.createElement('span');
        span.textContent = courseCode.toUpperCase();

        const button = document.createElement('button');
        button.className = 'remove-btn';
        button.innerHTML = '&times;'; // The 'x' character
        button.title = `Remove ${courseCode}`;
        
        // Add the event listener programmatically instead of using an inline onclick attribute
        button.addEventListener('click', () => removeCourse(courseCode));

        tag.appendChild(span);
        tag.appendChild(button);
        container.appendChild(tag);
    });
}

// Remove course from completed list
function removeCourse(courseCode) {
    const index = completedCourses.indexOf(courseCode);
    if (index !== -1) {
        completedCourses.splice(index, 1);
        updatePlanner();
    }
}

// Categorize courses into eligible and locked
function categorizeCoursesForPlanner() {
    const eligibleContainer = document.getElementById('eligible-courses');
    const lockedContainer = document.getElementById('locked-courses');

    eligibleContainer.innerHTML = '';
    lockedContainer.innerHTML = '';

    const coursesToCheck = allCourses.filter(course => !completedCourses.includes(course.course));

    coursesToCheck.forEach(async course => {
        try {
            const response = await fetch(`${API_BASE}/check`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    course: course.course,
                    finished: completedCourses
                })
            });

            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const result = await response.json();

            if (result.eligible) {
                const card = createCourseCard(course, 'eligible');
                eligibleContainer.appendChild(card);
            } else {
                const card = createCourseCard({ ...course, missing: result.missing }, 'locked');
                lockedContainer.appendChild(card);
            }
        } catch (error) {
            console.error('Error checking eligibility:', error);
            // Optionally, show an error state for this card
            const card = createCourseCard({ ...course, missing: ['check_failed'] }, 'locked');
            lockedContainer.appendChild(card);
        }
    });
}

// Create a course card element
function createCourseCard(course, type) {
    const card = document.createElement('div');
    card.className = `course-card ${type}`;

    let prereqText;
    if (type === 'locked' && course.missing) {
        prereqText = `Missing: ${course.missing.map(p => {
            if (p === 'max_electives') return 'Max electives taken';
            return p.toUpperCase();
        }).join(', ')}`;
    } else if (course.prerequisites.length > 0) {
        prereqText = `Requires: ${course.prerequisites.map(p => p.toUpperCase()).join(', ')}`;
    } else {
        prereqText = 'No prerequisites';
    }

    card.innerHTML = `
        <div class="course-code">${course.course.toUpperCase()}</div>
        <div class="course-prereqs">${prereqText}</div>
    `;

    if (type === 'eligible') {
        card.addEventListener('click', () => toggleCourse(course.course));
    }

    return card;
}

// Get prerequisite path
async function getPrerequisitePath() {
    const course = document.getElementById('path-course').value.trim().toLowerCase();
    const resultBox = document.getElementById('path-result');

    if (!course) {
        showResult(resultBox, 'error', 'Please enter a course code.');
        return;
    }

    try {
        const response = await fetch(`${API_BASE}/path`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ course })
        });
        const data = await response.json();

        let message = `<h3>Prerequisite Path for ${course.toUpperCase()}</h3>`;
        if (data.path.length > 0) {
            message += '<p>All courses in the prerequisite chain:</p><div>';
            data.path.forEach(p => {
                message += `<span class="path-item">${p.toUpperCase()}</span>`;
            });
            message += '</div>';
            showResult(resultBox, 'info', message);
        } else {
            message += '<p>No prerequisites found for this course.</p>';
            showResult(resultBox, 'info', message);
        }
    } catch (error) {
        console.error('Error getting prerequisite path:', error);
        showResult(resultBox, 'error', 'Failed to get prerequisite path. Make sure the server is running.');
    }
}

// Helper function to show results
function showResult(element, type, message) {
    element.className = `result-box show ${type}`;
    element.innerHTML = message;
}

// Helper function to show errors
function showError(message) {
    const container = document.querySelector('.container');
    const errorDiv = document.createElement('div');
    errorDiv.className = 'result-box show error';
    errorDiv.style.margin = '20px';
    errorDiv.innerHTML = `<strong>Error:</strong> ${message}`;
    container.insertBefore(errorDiv, container.firstChild);
}

// Make removeCourse available globally
window.removeCourse = removeCourse;