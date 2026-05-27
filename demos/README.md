# Pythia Anthropic VCL Demo

## GitHub MCP setup

The demo ships with a pre-wired GitHub MCP server entry, but it cannot connect to GitHub on its own — it needs a personal access token (PAT) issued from your GitHub account.

### 1. Create a Personal Access Token

1. Sign in to your GitHub account.
2. Open **https://github.com/settings/personal-access-tokens**.
3. Click **Generate new token** and follow the prompts. Pick the scopes you actually need; the demo does not require any specific scope by itself, the scopes you select will determine what the MCP server is allowed to do on your behalf.
4. Copy the generated token. GitHub only shows it once.

### 2. Register the token in the demo

Open the MCP cards configuration file shipped alongside the compiled demo:

```
bin64\VCL_Anthropic\support\VCL_Anthropic-mcp-cards.json
```

Locate the `github` entry and paste your PAT into the `pat` field, replacing `your github pat`:

```json
{
  "id": "github",
  "name": "Github",
  "commentaire": "GitHub access via PAT to be provided",
  "badge": "\uE186",
  "content": "{\"type\":\"url\",\"url\":\"https:\/\/api.githubcopilot.com/mcp/\",\"name\":\"Github\",\"authorization_token\":\"%s\"}",
  "pat": "your github pat"
}
```

Save the file. The next time the demo loads the MCP card, the PAT is substituted into the `authorization_token` placeholder of `content` and the GitHub MCP server becomes usable.

### A note on automation

This step is intentionally manual in the demo. In a production application you would typically wrap PAT entry behind a small UI (for example a settings dialog backed by the OS secret store) and have the application write the token into the card itself, rather than asking the user to edit a JSON file by hand. We did not implement that flow here so the configuration surface stays explicit and easy to inspect while reading the demo.

## Security reminder

A GitHub PAT grants real access to your GitHub account. Treat the JSON file as you would any credential file:

- Do not commit it with a real token inside.
- Do not share it.
- Revoke the token from the GitHub settings page as soon as you no longer need it.
