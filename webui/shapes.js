
shapes = {



};

shapes.Gate = joint.shapes.basic.Generic.extend({

  defaults: joint.util.deepSupplement({

    type: 'Gate',
    size: { width: 80, height: 40 },
    attrs: {
      '.': { magnet: false },
      '.body': { width: 100, height: 50 },
      circle: { r: 7, stroke: 'black', fill: 'transparent', 'stroke-width': 2 }
    }

  }, joint.shapes.basic.Generic.prototype.defaults),

  operation: function() { return true; }
});

shapes.Gate11 = shapes.Gate.extend({

  markup: '<g class="rotatable"><g class="scalable"><image class="body"/></g><circle class="input"/><circle class="output"/></g>',

  defaults: joint.util.deepSupplement({

    type: 'Gate11',
    attrs: {
      '.input': { ref: '.body', 'ref-x': -2, 'ref-y': 0.5, magnet: 'passive', port: 'in' },
      '.output': { ref: '.body', 'ref-dx': 2, 'ref-y': 0.5, magnet: true, port: 'out' }
    }

  }, shapes.Gate.prototype.defaults)
});

shapes.Gate21 = shapes.Gate.extend({

  markup: '<g class="rotatable"><g class="scalable"><image class="body"/><text/></g><circle class="input input1"/><circle  class="input input2"/><circle class="output"/></g>',

  defaults: joint.util.deepSupplement({

    type: 'logic.Gate21',
    attrs: {
      '.input1': { ref: '.body', 'ref-x': -2, 'ref-y': 0.3, magnet: 'passive', port: 'in1' },
      '.input2': { ref: '.body', 'ref-x': -2, 'ref-y': 0.7, magnet: 'passive', port: 'in2' },
      '.output': { ref: '.body', 'ref-dx': 2, 'ref-y': 0.5, magnet: true, port: 'out' }
    }

  }, shapes.Gate.prototype.defaults)

});

shapes.ConjI = shapes.Gate21.extend({

  defaults: joint.util.deepSupplement({

    type: 'conjI',
    attrs: {
      image: { 'xlink:href': 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+PHN2ZyAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIgICB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIiAgIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5zIyIgICB4bWxuczpzdmc9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiAgIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgICB4bWxuczpzb2RpcG9kaT0iaHR0cDovL3NvZGlwb2RpLnNvdXJjZWZvcmdlLm5ldC9EVEQvc29kaXBvZGktMC5kdGQiICAgeG1sbnM6aW5rc2NhcGU9Imh0dHA6Ly93d3cuaW5rc2NhcGUub3JnL25hbWVzcGFjZXMvaW5rc2NhcGUiICAgdmVyc2lvbj0iMS4xIiAgIGlkPSJzdmcyIiAgIHZpZXdCb3g9IjAgMCA3NDQuMDk0NDg4MTkgMTA1Mi4zNjIyMDQ3IiAgIGhlaWdodD0iMjk3bW0iICAgd2lkdGg9IjIxMG1tIiAgIGlua3NjYXBlOnZlcnNpb249IjAuOTEgcjEzNzI1IiAgIHNvZGlwb2RpOmRvY25hbWU9IkxvZ2lrMS5zdmciPiAgPHNvZGlwb2RpOm5hbWVkdmlldyAgICAgcGFnZWNvbG9yPSIjZmZmZmZmIiAgICAgYm9yZGVyY29sb3I9IiM2NjY2NjYiICAgICBib3JkZXJvcGFjaXR5PSIxIiAgICAgb2JqZWN0dG9sZXJhbmNlPSIxMCIgICAgIGdyaWR0b2xlcmFuY2U9IjEwIiAgICAgZ3VpZGV0b2xlcmFuY2U9IjEwIiAgICAgaW5rc2NhcGU6cGFnZW9wYWNpdHk9IjAiICAgICBpbmtzY2FwZTpwYWdlc2hhZG93PSIyIiAgICAgaW5rc2NhcGU6d2luZG93LXdpZHRoPSIxNjAwIiAgICAgaW5rc2NhcGU6d2luZG93LWhlaWdodD0iMTEzNyIgICAgIGlkPSJuYW1lZHZpZXcxMCIgICAgIHNob3dncmlkPSJ0cnVlIiAgICAgaW5rc2NhcGU6em9vbT0iMC40NDg1MTQ3OCIgICAgIGlua3NjYXBlOmN4PSIxNDIuMDA5NSIgICAgIGlua3NjYXBlOmN5PSI1MzAuMDE1NzkiICAgICBpbmtzY2FwZTp3aW5kb3cteD0iLTgiICAgICBpbmtzY2FwZTp3aW5kb3cteT0iLTgiICAgICBpbmtzY2FwZTp3aW5kb3ctbWF4aW1pemVkPSIxIiAgICAgaW5rc2NhcGU6Y3VycmVudC1sYXllcj0ic3ZnMiIgICAgIHNob3dndWlkZXM9InRydWUiPiAgICA8aW5rc2NhcGU6Z3JpZCAgICAgICB0eXBlPSJ4eWdyaWQiICAgICAgIGlkPSJncmlkNDEzNyIgLz4gIDwvc29kaXBvZGk6bmFtZWR2aWV3PiAgPGRlZnMgICAgIGlkPSJkZWZzNCIgLz4gIDxtZXRhZGF0YSAgICAgaWQ9Im1ldGFkYXRhNyI+ICAgIDxyZGY6UkRGPiAgICAgIDxjYzpXb3JrICAgICAgICAgcmRmOmFib3V0PSIiPiAgICAgICAgPGRjOmZvcm1hdD5pbWFnZS9zdmcreG1sPC9kYzpmb3JtYXQ+ICAgICAgICA8ZGM6dHlwZSAgICAgICAgICAgcmRmOnJlc291cmNlPSJodHRwOi8vcHVybC5vcmcvZGMvZGNtaXR5cGUvU3RpbGxJbWFnZSIgLz4gICAgICAgIDxkYzp0aXRsZSAvPiAgICAgIDwvY2M6V29yaz4gICAgPC9yZGY6UkRGPiAgPC9tZXRhZGF0YT4gIDxnICAgICBpZD0ibGF5ZXIxIiAgICAgdHJhbnNmb3JtPSJtYXRyaXgoMC4zNDM5OTgxMSwwLDAsMC4zNDM5OTgxMSw5MTMuMDMxNTcsMzExLjk5MzI3KSI+ICAgIDxwYXRoICAgICAgIGQ9Im0gLTE1NzcuMTQyOSwtNDEuMTIwNzI4IC0xNzguNTAxNSwtMzA5LjE3MzUyMiAtMTc4LjUwMTQsLTMwOS4xNzM1IDM1Ny4wMDI5LC0xMGUtNiAzNTcuMDAyOCwtMTBlLTYgLTE3OC41MDE0LDMwOS4xNzM1MyB6IiAgICAgICBpZD0icGF0aDQxMzYiICAgICAgIHN0eWxlPSJvcGFjaXR5OjE7ZmlsbDpub25lO2ZpbGwtb3BhY2l0eToxO2ZpbGwtcnVsZTpldmVub2RkO3N0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDo3NTtzdHJva2UtbGluZWNhcDpidXR0O3N0cm9rZS1saW5lam9pbjpyb3VuZDtzdHJva2UtbWl0ZXJsaW1pdDo0O3N0cm9rZS1kYXNoYXJyYXk6bm9uZTtzdHJva2UtZGFzaG9mZnNldDowO3N0cm9rZS1vcGFjaXR5OjEiICAgICAgIGlua3NjYXBlOmNvbm5lY3Rvci1jdXJ2YXR1cmU9IjAiIC8+ICAgIDxyZWN0ICAgICAgIHJ5PSI1LjQ5OTIyOSIgICAgICAgeT0iLTU1Ni43MzQyNSIgICAgICAgeD0iLTIzMDguNTQxIiAgICAgICBoZWlnaHQ9IjEwLjk5ODQ1OCIgICAgICAgd2lkdGg9IjQzNy4wNTkyMyIgICAgICAgaWQ9InJlY3Q0MTM4IiAgICAgICBzdHlsZT0ib3BhY2l0eToxO2ZpbGw6IzAwMDAwMDtmaWxsLW9wYWNpdHk6MTtmaWxsLXJ1bGU6ZXZlbm9kZDtzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MTQuOTgyMTkyOTk7c3Ryb2tlLWxpbmVjYXA6YnV0dDtzdHJva2UtbGluZWpvaW46cm91bmQ7c3Ryb2tlLW1pdGVybGltaXQ6NDtzdHJva2UtZGFzaGFycmF5Om5vbmU7c3Ryb2tlLWRhc2hvZmZzZXQ6MDtzdHJva2Utb3BhY2l0eToxIiAvPiAgICA8cmVjdCAgICAgICBzdHlsZT0ib3BhY2l0eToxO2ZpbGw6IzAwMDAwMDtmaWxsLW9wYWNpdHk6MTtmaWxsLXJ1bGU6ZXZlbm9kZDtzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MTU7c3Ryb2tlLWxpbmVjYXA6YnV0dDtzdHJva2UtbGluZWpvaW46cm91bmQ7c3Ryb2tlLW1pdGVybGltaXQ6NDtzdHJva2UtZGFzaGFycmF5Om5vbmU7c3Ryb2tlLWRhc2hvZmZzZXQ6MDtzdHJva2Utb3BhY2l0eToxIiAgICAgICBpZD0icmVjdDQxNDAiICAgICAgIHdpZHRoPSI0NzYuOTkzODciICAgICAgIGhlaWdodD0iMTAuNjY2MjA1IiAgICAgICB4PSItMTM2OS43NTU3IiAgICAgICB5PSItMzYzLjg1NjAyIiAgICAgICByeT0iNS4zMzMxMDI3IiAvPiAgICA8cmVjdCAgICAgICByeT0iNC42OTAwODU0IiAgICAgICB5PSItMTM0Ljc4MjI0IiAgICAgICB4PSItMjMwOC44MzU0IiAgICAgICBoZWlnaHQ9IjkuMzgwMTcwOCIgICAgICAgd2lkdGg9IjY3MC44MzA0NCIgICAgICAgaWQ9InJlY3Q0MTQyIiAgICAgICBzdHlsZT0ib3BhY2l0eToxO2ZpbGw6IzAwMDAwMDtmaWxsLW9wYWNpdHk6MTtmaWxsLXJ1bGU6ZXZlbm9kZDtzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MTQuNzU4NjY2MDQ7c3Ryb2tlLWxpbmVjYXA6YnV0dDtzdHJva2UtbGluZWpvaW46cm91bmQ7c3Ryb2tlLW1pdGVybGltaXQ6NDtzdHJva2UtZGFzaGFycmF5Om5vbmU7c3Ryb2tlLWRhc2hvZmZzZXQ6MDtzdHJva2Utb3BhY2l0eToxIiAvPiAgPC9nPjwvc3ZnPg==' },
      text: { text: 'conjI', 'ref-x': 27, 'ref-y': 0.5, 'font-size': 10 }
    }

  }, shapes.Gate21.prototype.defaults),

  operation: function(input) {
    return input;
  }

});
