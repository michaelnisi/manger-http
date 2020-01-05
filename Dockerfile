FROM node:12.14.0-buster-slim

ENV PORT 8080
WORKDIR /usr/src/app
COPY package*.json ./
RUN npm install
COPY . .

EXPOSE 8080
USER node
CMD [ "node", "start.js" ]