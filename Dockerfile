FROM node:8.16

ENV PORT 8080
WORKDIR /usr/src/app
COPY package*.json ./
RUN npm install
COPY . .

EXPOSE 8080
USER node
CMD [ "node", "--abort-on-uncaught-exception", "start.js" ]
