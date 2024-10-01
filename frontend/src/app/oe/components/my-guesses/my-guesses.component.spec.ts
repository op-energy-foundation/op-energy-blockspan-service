import { ComponentFixture, TestBed } from '@angular/core/testing';

import { MyGuessesComponent } from './my-guesses.component';

describe('MyGuessesComponent', () => {
  let component: MyGuessesComponent;
  let fixture: ComponentFixture<MyGuessesComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ MyGuessesComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(MyGuessesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
