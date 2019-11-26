'use strict'

// start - start manger-http service

new (require('./'))(require('./conf')).start()
