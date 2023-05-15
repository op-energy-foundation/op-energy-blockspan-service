import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BlockspanNavigatorComponent } from './blockspan-navigator.component';

describe('BlockspanNavigatorComponent', () => {
  let component: BlockspanNavigatorComponent;
  let fixture: ComponentFixture<BlockspanNavigatorComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [BlockspanNavigatorComponent],
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockspanNavigatorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
