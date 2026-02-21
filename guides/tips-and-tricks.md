# Tips and Tricks

- [How to prevent an error when closing an application while requests are still in progress](#how-to-prevent-an-error-when-closing-an-application-while-requests-are-still-in-progress)

___

<br>

## How to prevent an error when closing an application while requests are still in progress

Starting from **version 1.2.0 of AnthropicGemini**, the `Anthropic.Monitoring` unit is responsible for monitoring ongoing HTTP requests.

The Monitoring interface is accessible by including the Anthropic.Monitoring unit in the uses clause. Alternatively, you can access it via the HttpMonitoring function, declared in the `Anthropic` unit.

### Usage Exemple

```pascal
//uses Anthropic;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not HttpMonitoring.IsBusy;
  if not CanClose then
    MessageDLG(
      'Requests are still in progress. Please wait for them to complete before closing the application."',
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;
```