import { FlyingclubReserveClientPage } from './app.po';

describe('flyingclub-reserve-client App', () => {
  let page: FlyingclubReserveClientPage;

  beforeEach(() => {
    page = new FlyingclubReserveClientPage();
  });

  it('should display welcome message', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('Welcome to app!!');
  });
});
