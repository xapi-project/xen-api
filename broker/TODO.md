
Do we want 'freshly named' queues to be non-persistent?

Do we want non-persistent queues to be deleted when the last connection closes? This might happen prematurely in the web-browser case; would it be better to time out inactive sessions instead?

At what level will task management happen? Are progress updates sent as messages, or is progress more like a 'datasource' and read at will using shared memory? What about cancelling in-progress requests?

